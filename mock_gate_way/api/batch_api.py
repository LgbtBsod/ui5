import json
import re
import uuid
from dataclasses import dataclass
from urllib.parse import urlsplit, urlunsplit, parse_qsl, urlencode

import httpx
from fastapi import APIRouter, HTTPException, Request, Response

router = APIRouter(tags=["Batch"])


@dataclass
class BatchOperation:
    method: str
    path: str
    headers: dict[str, str]
    body: str


def _extract_boundary(content_type: str | None) -> str:
    if not content_type:
        raise HTTPException(status_code=400, detail="MISSING_CONTENT_TYPE")
    match = re.search(r"boundary=([^;]+)", content_type, flags=re.IGNORECASE)
    if not match:
        raise HTTPException(status_code=400, detail="MISSING_BATCH_BOUNDARY")
    return match.group(1).strip().strip('"')


def _normalize_linebreaks(payload: str) -> str:
    return payload.replace("\r\n", "\n")


def _parse_http_part(raw_part: str) -> BatchOperation:
    normalized = _normalize_linebreaks(raw_part).strip()
    if not normalized:
        raise HTTPException(status_code=400, detail="EMPTY_BATCH_PART")

    if "\n\n" not in normalized:
        raise HTTPException(status_code=400, detail="INVALID_BATCH_HTTP_PART")

    preamble, http_payload = normalized.split("\n\n", 1)
    if "application/http" not in preamble.lower():
        raise HTTPException(status_code=400, detail="UNSUPPORTED_BATCH_PART")

    lines = http_payload.split("\n")
    if not lines:
        raise HTTPException(status_code=400, detail="INVALID_BATCH_REQUEST")

    request_line = lines[0].strip()
    req_match = re.match(r"([A-Z]+)\s+([^\s]+)\s+HTTP/1\.[01]", request_line)
    if not req_match:
        raise HTTPException(status_code=400, detail="INVALID_BATCH_REQUEST_LINE")

    method = req_match.group(1)
    path = req_match.group(2)
    if path.startswith("http"):
        path = "/" + path.split("/", 3)[-1]

    headers: dict[str, str] = {}
    body_lines: list[str] = []
    current = 1
    in_body = False
    while current < len(lines):
        line = lines[current]
        current += 1
        if not in_body:
            if line.strip() == "":
                in_body = True
                continue
            if ":" in line:
                key, value = line.split(":", 1)
                headers[key.strip()] = value.strip()
        else:
            body_lines.append(line)

    return BatchOperation(method=method, path=path, headers=headers, body="\n".join(body_lines).strip())


def _parse_multipart(body: str, boundary: str) -> list[str]:
    marker = f"--{boundary}"
    chunks = _normalize_linebreaks(body).split(marker)
    parts: list[str] = []
    for chunk in chunks:
        stripped = chunk.strip()
        if not stripped or stripped == "--":
            continue
        if stripped.endswith("--"):
            stripped = stripped[:-2].strip()
        if stripped:
            parts.append(stripped)
    return parts




def _sanitize_operation_path(path: str) -> str:
    if not path:
        return path

    # Defensive normalization for mixed-locale encoding typos (%2с with cyrillic "с")
    path = path.replace("%2с", "%2c").replace("%2С", "%2C")

    parsed = urlsplit(path)
    query_items = parse_qsl(parsed.query, keep_blank_values=True)
    sanitized_items = []
    for key, value in query_items:
        if key == "$filter" and not str(value or "").strip():
            continue
        sanitized_items.append((key, value))

    query = urlencode(sanitized_items, doseq=True)
    return urlunsplit((parsed.scheme, parsed.netloc, parsed.path, query, parsed.fragment))


def _parse_batch_request(body: str, boundary: str) -> list[BatchOperation | list[BatchOperation]]:
    parsed: list[BatchOperation | list[BatchOperation]] = []
    for part in _parse_multipart(body, boundary):
        header_block, _, content = _normalize_linebreaks(part).partition("\n\n")
        header_lower = header_block.lower()
        if "multipart/mixed" in header_lower:
            nested_boundary = _extract_boundary(header_block)
            changeset_ops = [_parse_http_part(p) for p in _parse_multipart(content, nested_boundary)]
            parsed.append(changeset_ops)
        else:
            parsed.append(_parse_http_part(part))
    return parsed


async def _execute_operation(request: Request, op: BatchOperation) -> httpx.Response:
    if op.path.endswith("/$batch") or op.path == "/$batch":
        raise HTTPException(status_code=400, detail="NESTED_BATCH_NOT_SUPPORTED")

    content: bytes | None = None
    headers = {k: v for k, v in op.headers.items() if k.lower() not in {"host", "content-length"}}
    if op.body:
        content = op.body.encode("utf-8")
    transport = httpx.ASGITransport(app=request.app)
    async with httpx.AsyncClient(transport=transport, base_url="http://batch.local") as client:
        return await client.request(op.method, _sanitize_operation_path(op.path), headers=headers, content=content)


def _format_http_response(part_response: httpx.Response, content_id: str | None = None) -> str:
    headers = ["Content-Type: application/http", "Content-Transfer-Encoding: binary"]
    if content_id:
        headers.append(f"Content-ID: {content_id}")

    payload_lines = [f"HTTP/1.1 {part_response.status_code} {part_response.reason_phrase or ''}".rstrip()]

    content_type = part_response.headers.get("content-type")
    if content_type:
        payload_lines.append(f"Content-Type: {content_type}")
    payload_lines.append("")
    payload_text = part_response.text or ""
    payload_lines.append(payload_text)

    return "\r\n".join(headers) + "\r\n\r\n" + "\r\n".join(payload_lines)


def _encode_top_level_response(parts: list[str]) -> tuple[str, str]:
    resp_boundary = f"batchresponse_{uuid.uuid4().hex}"
    chunks = []
    for part in parts:
        chunks.append(f"--{resp_boundary}\r\n{part}\r\n")
    chunks.append(f"--{resp_boundary}--\r\n")
    return "".join(chunks), resp_boundary


@router.post("/$batch")
async def batch(request: Request):
    body_raw = await request.body()
    body = body_raw.decode("utf-8")
    boundary = _extract_boundary(request.headers.get("content-type"))

    operations = _parse_batch_request(body, boundary)

    response_parts: list[str] = []
    for item in operations:
        if isinstance(item, list):
            changeset_boundary = f"changesetresponse_{uuid.uuid4().hex}"
            changeset_chunks: list[str] = []
            for index, op in enumerate(item, start=1):
                op_response = await _execute_operation(request, op)
                changeset_chunks.append(
                    f"--{changeset_boundary}\r\n{_format_http_response(op_response, content_id=str(index))}\r\n"
                )
            changeset_chunks.append(f"--{changeset_boundary}--\r\n")
            response_parts.append(
                "\r\n".join([
                    f"Content-Type: multipart/mixed; boundary={changeset_boundary}",
                    "",
                    "".join(changeset_chunks),
                ]).rstrip()
            )
        else:
            op_response = await _execute_operation(request, item)
            response_parts.append(_format_http_response(op_response))

    payload, resp_boundary = _encode_top_level_response(response_parts)
    return Response(
        content=payload,
        media_type=f"multipart/mixed; boundary={resp_boundary}",
        headers={"DataServiceVersion": "2.0"}
    )


@router.post("/$batch/json")
async def batch_json(request: Request):
    """Debug helper to emulate grouped execution without MIME formatting."""
    payload = await request.json()
    calls = payload.get("operations") if isinstance(payload, dict) else None
    if not isinstance(calls, list):
        raise HTTPException(status_code=400, detail="INVALID_BATCH_JSON")

    results = []
    for op in calls:
        if not isinstance(op, dict):
            raise HTTPException(status_code=400, detail="INVALID_BATCH_OPERATION")
        operation = BatchOperation(
            method=str(op.get("method") or "GET").upper(),
            path=str(op.get("path") or "/"),
            headers={k: str(v) for k, v in (op.get("headers") or {}).items()},
            body=json.dumps(op.get("body")) if isinstance(op.get("body"), (dict, list)) else str(op.get("body") or ""),
        )
        resp = await _execute_operation(request, operation)
        results.append({"status": resp.status_code, "body": resp.json() if "json" in (resp.headers.get("content-type") or "") else resp.text})
    return {"d": {"results": results}}
