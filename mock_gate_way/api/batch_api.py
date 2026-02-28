import json
import uuid
from urllib.parse import urlsplit, urlunsplit, parse_qsl, urlencode

import httpx
from fastapi import APIRouter, HTTPException, Request, Response

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


async def _execute_operation(request: Request, op: BatchOperation) -> httpx.Response:
    if op.path.endswith("/$batch") or op.path == "/$batch":
        raise HTTPException(status_code=400, detail="NESTED_BATCH_NOT_SUPPORTED")

    headers = {k: v for k, v in op.headers.items() if k.lower() not in {"host", "content-length"}}
    if "X-CSRF-Token" not in headers and request.headers.get("X-CSRF-Token"):
        headers["X-CSRF-Token"] = request.headers.get("X-CSRF-Token")
    if "cookie" not in {k.lower() for k in headers} and request.headers.get("cookie"):
        headers["Cookie"] = request.headers.get("cookie")
    content = op.body.encode("utf-8") if op.body else None
    transport = httpx.ASGITransport(app=request.app)
    async with httpx.AsyncClient(transport=transport, base_url="http://batch.local") as client:
        return await client.request(op.method, _sanitize_operation_path(op.path), headers=headers, content=content)


@router.post("/$batch")
async def batch(request: Request):
    boundary = extract_boundary(request.headers.get("content-type"))
    operations = parse_batch_request((await request.body()).decode("utf-8"), boundary)

    response_parts: list[str] = []
    for item in operations:
        if isinstance(item, list):
            changeset_boundary = f"changesetresponse_{uuid.uuid4().hex}"
            chunks: list[str] = []
            for i, op in enumerate(item, start=1):
                op_response = await _execute_operation(request, op)
                if op_response.status_code >= 400:
                    raise HTTPException(status_code=op_response.status_code, detail=op_response.text)
                cid = op.headers.get("Content-ID") or str(i)
                chunks.append(f"--{changeset_boundary}\r\n" + format_http_response(op_response.status_code, op_response.reason_phrase or "", op_response.headers.get("content-type") or "application/json", op_response.text or "", cid) + "\r\n")
            chunks.append(f"--{changeset_boundary}--\r\n")
            response_parts.append("\r\n".join([f"Content-Type: multipart/mixed; boundary={changeset_boundary}", "", "".join(chunks)]).rstrip())
        else:
            op_response = await _execute_operation(request, item)
            response_parts.append(format_http_response(op_response.status_code, op_response.reason_phrase or "", op_response.headers.get("content-type") or "application/json", op_response.text or ""))

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
