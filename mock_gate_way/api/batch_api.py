import json
import os
import uuid
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit

import httpx
from fastapi import APIRouter, HTTPException, Request, Response

from config import DATABASE_URL
from utils.odata_batch import (
    BatchOperation,
    encode_top_level,
    extract_boundary,
    format_http_response,
    parse_batch_request,
)

router = APIRouter(tags=["Batch"])


def _sanitize_operation_path(path: str) -> str:
    if not path:
        return path
    path = path.replace("%2с", "%2c").replace("%2С", "%2C")
    parsed = urlsplit(path)
    query_items = parse_qsl(parsed.query, keep_blank_values=True)
    sanitized_items = [(k, v) for k, v in query_items if not (k == "$filter" and not str(v or "").strip())]
    query = urlencode(sanitized_items, doseq=True)
    return urlunsplit((parsed.scheme, parsed.netloc, parsed.path, query, parsed.fragment))


def _sqlite_db_path() -> str | None:
    if not DATABASE_URL.startswith("sqlite:///"):
        return None
    return DATABASE_URL.replace("sqlite:///", "", 1)


def _snapshot_db() -> bytes | None:
    db_path = _sqlite_db_path()
    if not db_path:
        return None
    try:
        with open(db_path, "rb") as fh:
            return fh.read()
    except FileNotFoundError:
        return None


def _restore_db(snapshot: bytes | None) -> None:
    db_path = _sqlite_db_path()
    if not db_path or snapshot is None:
        return
    with open(db_path, "wb") as fh:
        fh.write(snapshot)
    wal_path = f"{db_path}-wal"
    shm_path = f"{db_path}-shm"
    for sidecar in (wal_path, shm_path):
        if os.path.exists(sidecar):
            os.remove(sidecar)


async def _execute_operation(request: Request, op: BatchOperation) -> httpx.Response:
    if op.path.endswith("/$batch") or op.path == "/$batch":
        raise HTTPException(status_code=400, detail="NESTED_BATCH_NOT_SUPPORTED")

    method = "PATCH" if op.method == "MERGE" else op.method
    headers = {k: v for k, v in op.headers.items() if k.lower() not in {"host", "content-length"}}
    if "X-CSRF-Token" not in headers and request.headers.get("X-CSRF-Token"):
        headers["X-CSRF-Token"] = request.headers.get("X-CSRF-Token")
    if "cookie" not in {k.lower() for k in headers} and request.headers.get("cookie"):
        headers["Cookie"] = request.headers.get("cookie")
    content = op.body.encode("utf-8") if op.body else None
    transport = httpx.ASGITransport(app=request.app)
    async with httpx.AsyncClient(transport=transport, base_url="http://batch.local") as client:
        return await client.request(method, _sanitize_operation_path(op.path), headers=headers, content=content)


@router.post("/$batch")
async def batch(request: Request):
    boundary = extract_boundary(request.headers.get("content-type"))
    operations = parse_batch_request((await request.body()).decode("utf-8"), boundary)

    response_parts: list[str] = []
    for item in operations:
        if isinstance(item, list):
            changeset_boundary = f"changesetresponse_{uuid.uuid4().hex}"
            chunks: list[str] = []
            db_snapshot = _snapshot_db()
            had_error = False
            for i, op in enumerate(item, start=1):
                op_response = await _execute_operation(request, op)
                cid = op.headers.get("Content-ID") or str(i)
                chunks.append(
                    f"--{changeset_boundary}\r\n"
                    + format_http_response(
                        op_response.status_code,
                        op_response.reason_phrase or "",
                        op_response.headers.get("content-type") or "application/json",
                        op_response.text or "",
                        cid,
                    )
                    + "\r\n"
                )
                if op_response.status_code >= 400:
                    had_error = True
                    _restore_db(db_snapshot)
                    break
            chunks.append(f"--{changeset_boundary}--\r\n")
            response_parts.append("\r\n".join([f"Content-Type: multipart/mixed; boundary={changeset_boundary}", "", "".join(chunks)]).rstrip())
            if had_error:
                continue
        else:
            op_response = await _execute_operation(request, item)
            response_parts.append(
                format_http_response(
                    op_response.status_code,
                    op_response.reason_phrase or "",
                    op_response.headers.get("content-type") or "application/json",
                    op_response.text or "",
                )
            )

    payload, resp_boundary = encode_top_level(response_parts)
    return Response(content=payload, media_type=f"multipart/mixed; boundary={resp_boundary}", headers={"DataServiceVersion": "2.0"})


@router.post("/$batch/json")
async def batch_json(request: Request):
    payload = await request.json()
    calls = payload.get("operations") if isinstance(payload, dict) else None
    if not isinstance(calls, list):
        raise HTTPException(status_code=400, detail="INVALID_BATCH_JSON")

    results = []
    for op in calls:
        operation = BatchOperation(
            method=str(op.get("method") or "GET").upper(),
            path=str(op.get("path") or "/"),
            headers={k: str(v) for k, v in (op.get("headers") or {}).items()},
            body=json.dumps(op.get("body")) if isinstance(op.get("body"), (dict, list)) else str(op.get("body") or ""),
        )
        resp = await _execute_operation(request, operation)
        results.append({"status": resp.status_code, "body": resp.text})
    return {"d": {"results": results}}
