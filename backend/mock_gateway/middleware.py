import json
import logging
from fastapi import Request, Response
from utils.odata import SERVICE_ROOT, odata_error_response
from utils.sap_message import build_sap_message

logger = logging.getLogger("gateway")

_LOG_BODY_METHODS = {"POST", "PUT", "PATCH", "MERGE", "DELETE"}
_LOG_BODY_CONTENT_TYPES = (
    "application/json",
    "application/xml",
    "text/plain",
    "multipart/mixed",
)
_LOG_BODY_MAX_CHARS = 4000


def _resolve_effective_method(request: Request) -> str:
    """SAP Gateway allows X-HTTP-Method override for tunnelling through proxies."""
    override = request.headers.get("X-HTTP-Method", "").upper()
    return override if override in {"GET", "POST", "PUT", "PATCH", "MERGE", "DELETE"} else request.method.upper()


def _trim_for_log(s_value: str, i_limit: int = _LOG_BODY_MAX_CHARS) -> str:
    return s_value[:i_limit] + "..." if len(s_value) > i_limit else s_value


def _format_batch_operations_for_log(s_body: str, s_content_type: str) -> list[str]:
    """Summarize $batch request body instead of logging full content."""
    if not s_body or "multipart/mixed" not in s_content_type.lower():
        return [s_body or ""]
    lines = []
    for line in s_body.split("\n"):
        if line.strip().startswith(("POST", "PUT", "PATCH", "MERGE", "DELETE", "GET")):
            lines.append(line.strip())
    return lines if lines else [f"($batch with {len(s_body)} bytes)"]


def _build_request_log_lines(request: Request, b_content_forced: bool, s_body_text: str | None, s_effective_method: str) -> list[str]:
    lines = [f"REQ {s_effective_method} {request.url.path}"]
    if b_content_forced and s_body_text:
        s_content_type = str(request.headers.get("content-type") or "").lower()
        batch_lines = _format_batch_operations_for_log(s_body_text, s_content_type)
        for batch_line in batch_lines:
            lines.append(f"    {_trim_for_log(batch_line)}")
    return lines


async def setup_csrf_middleware(app, csrf_store):
    """Register CSRF validation/issuance middleware."""
    @app.middleware("http")
    async def odata_csrf_middleware(request: Request, call_next):
        path = request.url.path
        is_odata = path.startswith(SERVICE_ROOT)
        if not is_odata:
            return await call_next(request)

        session_id = request.cookies.get("SAP_SESSIONID")
        token = request.headers.get("X-CSRF-Token")
        is_fetch = str(token).lower() == "fetch"

        # Both $batch entry points carry their own sub-operations, each with its own
        # method - CSRF for those is enforced by batch_api._enforce_batch_csrf() (which
        # only requires a token when the batch actually contains a write), not here.
        # This used to check only path.endswith("/$batch"), silently missing "/$batch/json"
        # and rejecting every batch_json call - including pure-read batches - with a
        # blanket CSRF_TOKEN_MISSING before the request ever reached batch_api.py.
        b_is_batch = path.endswith("/$batch") or path.endswith("/$batch/json")
        if request.method in {"POST", "PUT", "PATCH", "MERGE", "DELETE"} and not is_fetch and not b_is_batch:
            if not csrf_store.validate(session_id, token):
                return odata_error_response(403, "CSRF_TOKEN_MISSING", "CSRF token validation failed")

        response = await call_next(request)

        if is_fetch:
            sid, new_token = csrf_store.issue(session_id)
            response.headers["X-CSRF-Token"] = new_token
            # secure=True would make the browser silently drop this cookie on a plain-HTTP
            # origin (e.g. local dev on http://localhost:8080), breaking the whole CSRF/lock
            # flow with no visible error. Only require Secure when the request itself arrived over TLS.
            response.set_cookie(
                "SAP_SESSIONID", sid, httponly=True, secure=request.url.scheme == "https", samesite="lax"
            )
        return response


async def setup_odata_headers_middleware(app):
    """Register OData v2 response headers middleware."""
    @app.middleware("http")
    async def odata_response_headers_middleware(request: Request, call_next):
        response = await call_next(request)
        if request.url.path.startswith(SERVICE_ROOT):
            if "DataServiceVersion" not in response.headers:
                response.headers["DataServiceVersion"] = "2.0"
            if "MaxDataServiceVersion" not in response.headers:
                response.headers["MaxDataServiceVersion"] = "2.0"
            ct = response.headers.get("content-type", "")
            if ct and "application/json" in ct and "charset" not in ct:
                response.headers["content-type"] = ct.split(";")[0] + "; charset=utf-8"
            if "sap-server" not in response.headers:
                response.headers["sap-server"] = "mock/1.0"
            if "sap-message" not in response.headers and hasattr(response, "body") and response.status_code < 400:
                try:
                    payload = json.loads((response.body or b"{}").decode("utf-8"))
                except Exception:
                    payload = {}
                data = payload.get("d") if isinstance(payload, dict) else None
                if isinstance(data, dict) and data.get("Message"):
                    response.headers["sap-message"] = build_sap_message(data.get("Message"), "success", code=str(data.get("ReasonCode") or ""))
        return response


async def setup_logging_middleware(app, log_request_bodies: bool):
    """Register request/response logging middleware."""
    @app.middleware("http")
    async def request_response_logging(request: Request, call_next):
        s_effective_method = _resolve_effective_method(request)
        if s_effective_method != request.method.upper():
            request.scope["method"] = s_effective_method

        s_content_type = str(request.headers.get("content-type") or "").lower()
        b_method_with_body = log_request_bodies and s_effective_method in _LOG_BODY_METHODS
        b_supported_content_type = any(s_item in s_content_type for s_item in _LOG_BODY_CONTENT_TYPES)
        b_read_body = b_method_with_body and b_supported_content_type

        s_body_text: str | None = None
        if b_read_body:
            body_bytes = await request.body()
            s_body_text = body_bytes.decode("utf-8", errors="replace")

        for s_line in _build_request_log_lines(request, b_read_body, s_body_text, s_effective_method):
            logger.info(s_line)

        response = await call_next(request)
        logger.info("RES %s %s status=%s", request.method, request.url.path, response.status_code)
        return response


async def setup_error_envelope_middleware(app):
    """Register OData error envelope middleware."""
    @app.middleware("http")
    async def odata_error_envelope_middleware(request: Request, call_next):
        path = request.url.path
        is_odata = path.startswith(SERVICE_ROOT)
        if not is_odata:
            return await call_next(request)

        def _map_status_to_code(i_status: int) -> str:
            if i_status == 404:
                return "NOT_FOUND"
            if i_status == 400:
                return "VALIDATION_ERROR"
            if i_status == 403:
                return "FORBIDDEN"
            if i_status == 409:
                return "CONFLICT"
            return "SERVER_ERROR" if i_status >= 500 else "REQUEST_ERROR"

        response = await call_next(request)
        if 400 <= response.status_code < 600:
            # call_next always yields a streaming response, so the body must be drained
            # from body_iterator (response.body does not exist on that type).
            body_bytes = b"".join([chunk async for chunk in response.body_iterator])
            try:
                body = json.loads(body_bytes.decode("utf-8"))
                if isinstance(body, dict) and "error" not in body:
                    code = _map_status_to_code(response.status_code)
                    message = body.get("detail") or body.get("message") or "Request failed"
                    return odata_error_response(response.status_code, code, message)
            except Exception:
                pass
            passthrough_headers = {k: v for k, v in response.headers.items() if k.lower() != "content-length"}
            return Response(
                content=body_bytes,
                status_code=response.status_code,
                headers=passthrough_headers,
                media_type=response.media_type,
            )
        return response


def setup_rate_limit_middleware(app) -> None:
    """Configure per-IP rate limiting (DDoS protection complement to CSRF)."""
    from utils.rate_limiter import RateLimiter, extract_client_ip
    from fastapi import Response
    from config import RATE_LIMIT_REQUESTS_PER_WINDOW, RATE_LIMIT_WINDOW_SECONDS, RATE_LIMIT_TRUSTED_PROXIES

    limiter = RateLimiter(requests_per_window=RATE_LIMIT_REQUESTS_PER_WINDOW, window_seconds=RATE_LIMIT_WINDOW_SECONDS)
    app.state.rate_limiter = limiter

    @app.middleware("http")
    async def rate_limit_middleware(request: Request, call_next):
        client_ip = extract_client_ip(request, RATE_LIMIT_TRUSTED_PROXIES)
        allowed, headers = limiter.is_allowed(client_ip)

        if not allowed:
            logger.warning(f"Rate limit exceeded for {client_ip}")
            response = Response(
                content='{"error": {"code": "RATE_LIMIT_EXCEEDED", "message": "Too many requests"}}',
                status_code=429,
                media_type="application/json",
            )
            for key, value in headers.items():
                response.headers[key] = value
            return response

        response = await call_next(request)
        for key, value in headers.items():
            response.headers[key] = value
        return response
