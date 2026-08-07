from __future__ import annotations

import uuid
from dataclasses import dataclass
from typing import Pattern

from fastapi import HTTPException

from utils.patterns import (
    ODATA_BATCH_REQUEST_LINE_PATTERN,
    ODATA_BOUNDARY_PATTERN,
    make_odata_multipart_marker,
)


@dataclass
class BatchOperation:
    """Represents a parsed HTTP operation from OData batch request."""

    method: str
    path: str
    headers: dict[str, str]
    body: str


def extract_boundary(content_type: str | None) -> str:
    """Extract boundary string from Content-Type header.
    
    Args:
        content_type: Content-Type header value
        
    Returns:
        Boundary string without quotes
        
    Raises:
        HTTPException: If content type is missing or boundary not found
    """
    if not content_type:
        raise HTTPException(status_code=400, detail="MISSING_CONTENT_TYPE")
    
    match = ODATA_BOUNDARY_PATTERN.search(content_type)
    if not match:
        raise HTTPException(status_code=400, detail="MISSING_BATCH_BOUNDARY")
    
    return (match.group(1) or match.group(2) or "").strip()


def normalize_linebreaks(payload: str) -> str:
    """Normalize line breaks to Unix-style (LF only).
    
    Args:
        payload: Raw string with potential CRLF line breaks
        
    Returns:
        String with all line breaks converted to LF
    """
    return payload.replace("\r\n", "\n")


def parse_multipart(body: str, boundary: str) -> list[str]:
    """Parse multipart body using RFC 2046 compliant boundary detection.
    
    RFC 2046: a boundary delimiter is only significant at the start of a line 
    (preceded by a line break, or the very start of the body). A bare substring 
    split (the previous implementation) would corrupt parsing if a part's own 
    content happened to contain the literal "--{boundary}" text anywhere - 
    e.g. inside a JSON body value.
    
    Args:
        body: Raw multipart body string
        boundary: Boundary delimiter string
        
    Returns:
        List of part contents
    """
    marker: Pattern[str] = make_odata_multipart_marker(boundary)
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
    """Parse a single HTTP part from OData batch request.
    
    Args:
        raw_part: Raw part content including headers and body
        
    Returns:
        BatchOperation with parsed method, path, headers, and body
        
    Raises:
        HTTPException: If part format is invalid
    """
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
    while req_line_index < len(lines):
        candidate = lines[req_line_index].strip().lstrip("\ufeff")
        if not candidate:
            req_line_index += 1
            continue
        req_match = ODATA_BATCH_REQUEST_LINE_PATTERN.match(candidate)
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
    for line in lines[req_line_index + 1:]:
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
    """Parse complete OData batch request into operations.
    
    Args:
        body: Raw batch request body
        boundary: Boundary delimiter string
        
    Returns:
        List of BatchOperation objects (or nested lists for changesets)
    """
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
    """Normalize Content-Type header value.
    
    Args:
        content_type: Raw Content-Type value
        
    Returns:
        Normalized Content-Type with charset if JSON
    """
    value = str(content_type or "application/json").strip() or "application/json"
    if value.lower().startswith("application/json") and "charset=" not in value.lower():
        return "application/json; charset=utf-8"
    return value


def format_http_response(
    status_code: int, reason: str, content_type: str, body: str, content_id: str | None = None
) -> str:
    """Format HTTP response for OData batch response.
    
    Args:
        status_code: HTTP status code
        reason: Status reason phrase
        content_type: Response Content-Type
        body: Response body content
        content_id: Optional Content-ID header value
        
    Returns:
        Formatted HTTP response string
    """
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
    """Encode multiple response parts into batch response format.
    
    Args:
        parts: List of formatted HTTP response strings
        
    Returns:
        Tuple of (encoded body, boundary string)
    """
    boundary = f"batch_{uuid.uuid4().hex}"
    chunks = [f"--{boundary}\r\n{part}\r\n" for part in parts]
    chunks.append(f"--{boundary}--\r\n")
    return "".join(chunks), boundary
