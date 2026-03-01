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
    match = re.search(r"boundary=([^;]+)", content_type, flags=re.IGNORECASE)
    if not match:
        raise HTTPException(status_code=400, detail="MISSING_BATCH_BOUNDARY")
    return match.group(1).strip().strip('"')


def normalize_linebreaks(payload: str) -> str:
    return payload.replace("\r\n", "\n")


def parse_multipart(body: str, boundary: str) -> list[str]:
    marker = f"--{boundary}"
    chunks = normalize_linebreaks(body).split(marker)
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
    req_match = re.match(r"([A-Z]+)\s+([^\s]+)\s+HTTP/1\.[01]", lines[0].strip())
    if not req_match:
        raise HTTPException(status_code=400, detail="INVALID_BATCH_REQUEST_LINE")
    method, path = req_match.group(1), req_match.group(2)
    headers, body_lines, in_body = {}, [], False
    for line in lines[1:]:
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


def format_http_response(status_code: int, reason: str, content_type: str, body: str, content_id: str | None = None) -> str:
    headers = ["Content-Type: application/http", "Content-Transfer-Encoding: binary"]
    if content_id:
        headers.append(f"Content-ID: {content_id}")
    payload = [f"HTTP/1.1 {status_code} {reason}".rstrip(), f"Content-Type: {content_type}", "", body or ""]
    return "\r\n".join(headers) + "\r\n\r\n" + "\r\n".join(payload)


def encode_top_level(parts: list[str]) -> tuple[str, str]:
    boundary = f"batch_{uuid.uuid4().hex}"
    chunks = [f"--{boundary}\r\n{part}\r\n" for part in parts]
    chunks.append(f"--{boundary}--\r\n")
    return "".join(chunks), boundary
