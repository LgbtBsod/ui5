import json
import uuid
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit

import httpx
from fastapi import APIRouter, HTTPException, Request, Response
from database import SessionLocal, use_shared_session
from utils.odata import SERVICE_ROOT
from utils.odata_batch import (
    BatchOperation,
    encode_top_level,
    extract_boundary,
    format_http_response,
    parse_batch_request,
)

router = APIRouter(tags=["Batch"])


_WRITE_METHODS = {"POST", "PUT", "PATCH", "MERGE", "DELETE"}


def _has_write_operations(operations: list[BatchOperation | list[BatchOperation]]) -> bool:
    for item in operations:
        if isinstance(item, list):
            for op in item:
                if str(op.method or "").upper() in _WRITE_METHODS:
                    return True
            continue
        if str(item.method or "").upper() in _WRITE_METHODS:
            return True
    return False


def _enforce_batch_csrf(request: Request, operations: list[BatchOperation | list[BatchOperation]]) -> None:
    """Single source of truth for CSRF enforcement across both $batch entry points.
    odata_csrf_middleware skips /$batch* paths entirely (batch bodies carry their own
    sub-operations), so every batch route must call this explicitly."""
    if not _has_write_operations(operations):
        return
    csrf_store = getattr(request.app.state, "csrf_store", None)
    session_id = request.cookies.get("SAP_SESSIONID")
    token = request.headers.get("X-CSRF-Token")
    if not csrf_store or not csrf_store.validate(session_id, token):
        raise HTTPException(status_code=403, detail="CSRF_TOKEN_MISSING")


def _sanitize_operation_path(path: str) -> str:
    if not path:
        return path
    path = path.replace("%2с", "%2c").replace("%2С", "%2C")
    parsed = urlsplit(path)
    query_items = parse_qsl(parsed.query, keep_blank_values=True)
    sanitized_items = [(k, v) for k, v in query_items if not (k == "$filter" and not str(v or "").strip())]
    query = urlencode(sanitized_items, doseq=True)
    return urlunsplit((parsed.scheme, parsed.netloc, parsed.path, query, parsed.fragment))


def _resolve_service_root_path(path: str) -> str:
    parsed = urlsplit(str(path or ""))
    if parsed.scheme or parsed.netloc:
        return str(path or "")

    normalized_path = parsed.path or ""
    if not normalized_path.startswith("/"):
        normalized_path = "/" + normalized_path
    if normalized_path != "/$batch" and normalized_path != SERVICE_ROOT and not normalized_path.startswith(SERVICE_ROOT + "/"):
        normalized_path = SERVICE_ROOT + normalized_path
    return urlunsplit((parsed.scheme, parsed.netloc, normalized_path, parsed.query, parsed.fragment))


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
    target_path = _resolve_service_root_path(_sanitize_operation_path(op.path))
    transport = httpx.ASGITransport(app=request.app)
    async with httpx.AsyncClient(transport=transport, base_url="http://batch.local") as client:
        return await client.request(method, target_path, headers=headers, content=content)


@router.post("/$batch")
async def batch(request: Request):
    boundary = extract_boundary(request.headers.get("content-type"))
    operations = parse_batch_request((await request.body()).decode("utf-8"), boundary)
    _enforce_batch_csrf(request, operations)

    response_parts: list[str] = []
    for item in operations:
        if isinstance(item, list):
            changeset_boundary = f"changesetresponse_{uuid.uuid4().hex}"
            chunks: list[str] = []
            had_error = False
            # All operations in a changeset share one Session/transaction so the whole
            # changeset can be rolled back atomically (OData $batch changeset semantics:
            # all-or-nothing) - see database.use_shared_session. Each op still calls
            # db.commit()/db.rollback() as normal inside its own route handler; while its
            # SAVEPOINT is open, session.commit/session.rollback are redirected to that
            # savepoint's own commit/rollback so the handler's call only releases/undoes
            # its own op, not the outer transaction - SQLAlchemy 2.0's Session.commit()
            # otherwise always ends the *entire* transaction stack, nested or not.
            session = SessionLocal()
            original_commit, original_rollback = session.commit, session.rollback
            try:
                with use_shared_session(session):
                    for i, op in enumerate(item, start=1):
                        savepoint = session.begin_nested()
                        session.commit, session.rollback = savepoint.commit, savepoint.rollback
                        try:
                            op_response = await _execute_operation(request, op)
                        finally:
                            session.commit, session.rollback = original_commit, original_rollback
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
                            break
                if had_error:
                    session.rollback()
                else:
                    session.commit()
            finally:
                session.close()
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

    operations = [
        BatchOperation(
            method=str(op.get("method") or "GET").upper(),
            path=str(op.get("path") or "/"),
            headers={k: str(v) for k, v in (op.get("headers") or {}).items()},
            body=json.dumps(op.get("body")) if isinstance(op.get("body"), (dict, list)) else str(op.get("body") or ""),
        )
        for op in calls
    ]
    _enforce_batch_csrf(request, operations)

    results = []
    for operation in operations:
        resp = await _execute_operation(request, operation)
        results.append({"status": resp.status_code, "body": resp.text})
    return {"d": {"results": results}}
