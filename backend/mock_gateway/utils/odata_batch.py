from __future__ import annotations

import re
import uuid
from dataclasses import dataclass

from fastapi import HTTPException


@dataclass
class BatchOperation:
    method: str
    path: str
    headers: dict[str, str]
    body: str


def extract_boundary(content_type: str | None) -> str:
    if not content_type:
        raise HTTPException(status_code=400, detail="MISSING_CONTENT_TYPE")
    match = re.search(r'boundary=(?:"([^"]+)"|([^\s;]+))', content_type, flags=re.IGNORECASE)
    if not match:
        raise HTTPException(status_code=400, detail="MISSING_BATCH_BOUNDARY")
    return (match.group(1) or match.group(2) or "").strip()


def normalize_linebreaks(payload: str) -> str:
    return payload.replace("\r\n", "\n")


def parse_multipart(body: str, boundary: str) -> list[str]:
    # RFC 2046: a boundary delimiter is only significant at the start of a line (preceded
    # by a line break, or the very start of the body). A bare substring split (the
    # previous implementation) would corrupt parsing if a part's own content happened to
    # contain the literal "--{boundary}" text anywhere - e.g. inside a JSON body value.
    marker = re.compile(rf"(?:\A|\n)--{re.escape(boundary)}")
    chunks = marker.split(normalize_linebreaks(body))
    parts = []
    for chunk in chunks:
        stripped = chunk.strip()
        if not stripped or stripped == "--":
            continue
        if stripped.endswith("--"):
            stripped = stripped[:-2].strip()
        if stripped:
            parts.append(stripped)
    return parts


def parse_http_part(raw_part: str) -> BatchOperation:
    normalized = normalize_linebreaks(raw_part).strip()
    if "\n\n" not in normalized:
        raise HTTPException(status_code=400, detail="INVALID_BATCH_HTTP_PART")
    preamble, http_payload = normalized.split("\n\n", 1)
    if "application/http" not in preamble.lower():
        raise HTTPException(status_code=400, detail="UNSUPPORTED_BATCH_PART")
    lines = http_payload.split("\n")
    while lines and not lines[0].strip():
        lines.pop(0)
    if not lines:
        raise HTTPException(status_code=400, detail="INVALID_BATCH_REQUEST_LINE")

    req_line_index = 0
    req_match = None
    request_line_pattern = re.compile(r"^([A-Z]+)\s+(.+?)\s*(?:HTTP/\d(?:\.\d+)?)?$", flags=re.IGNORECASE)
    while req_line_index < len(lines):
        candidate = lines[req_line_index].strip().lstrip("\ufeff")
        if not candidate:
            req_line_index += 1
            continue
        req_match = request_line_pattern.match(candidate)
        if req_match:
            break
        if ":" in candidate:
            req_line_index += 1
            continue
        req_match = None
        break
    if not req_match:
        raise HTTPException(status_code=400, detail="INVALID_BATCH_REQUEST_LINE")
    method, path = req_match.group(1).upper(), req_match.group(2).strip()
    headers, body_lines, in_body = {}, [], False
    for line in lines[req_line_index + 1 :]:
        if not in_body:
            if line.strip() == "":
                in_body = True
                continue
            if ":" in line:
                k, v = line.split(":", 1)
                headers[k.strip()] = v.strip()
        else:
            body_lines.append(line)
    return BatchOperation(method=method, path=path, headers=headers, body="\n".join(body_lines).strip())


def parse_batch_request(body: str, boundary: str) -> list[BatchOperation | list[BatchOperation]]:
    parsed = []
    for part in parse_multipart(body, boundary):
        header_block, _, content = normalize_linebreaks(part).partition("\n\n")
        if "multipart/mixed" in header_block.lower():
            nested = extract_boundary(header_block)
            parsed.append([parse_http_part(p) for p in parse_multipart(content, nested)])
        else:
            parsed.append(parse_http_part(part))
    return parsed


def _normalize_content_type(content_type: str | None) -> str:
    value = str(content_type or "application/json").strip() or "application/json"
    if value.lower().startswith("application/json") and "charset=" not in value.lower():
        return "application/json; charset=utf-8"
    return value


def format_http_response(
    status_code: int, reason: str, content_type: str, body: str, content_id: str | None = None
) -> str:
    body_text = body or ""
    payload_type = _normalize_content_type(content_type)
    headers = ["Content-Type: application/http", "Content-Transfer-Encoding: binary"]
    if content_id:
        headers.append(f"Content-ID: {content_id}")
    payload = [
        f"HTTP/1.1 {status_code} {reason}".rstrip(),
        f"Content-Type: {payload_type}",
        "DataServiceVersion: 2.0",
        "MaxDataServiceVersion: 2.0",
        f"Content-Length: {len(body_text.encode('utf-8'))}",
        "",
        body_text,
    ]
    return "\r\n".join(headers) + "\r\n\r\n" + "\r\n".join(payload)


def encode_top_level(parts: list[str]) -> tuple[str, str]:
    boundary = f"batch_{uuid.uuid4().hex}"
    chunks = [f"--{boundary}\r\n{part}\r\n" for part in parts]
    chunks.append(f"--{boundary}--\r\n")
    return "".join(chunks), boundary
