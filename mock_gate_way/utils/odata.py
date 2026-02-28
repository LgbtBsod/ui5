from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Any

from fastapi.responses import JSONResponse

SERVICE_ROOT = "/sap/opu/odata/sap/Z_UI5_SRV"
ODATA_NS = "Z_UI5_SRV"


def format_datetime(value: datetime | None) -> str | None:
    if value is None:
        return None
    dt = value if value.tzinfo else value.replace(tzinfo=timezone.utc)
    millis = int(dt.astimezone(timezone.utc).timestamp() * 1000)
    return f"/Date({millis})/"


def format_entity_etag(version_number: int | None, changed_on: datetime | None) -> str | None:
    if changed_on is None:
        return None
    dt = changed_on if changed_on.tzinfo else changed_on.replace(tzinfo=timezone.utc)
    ms = int(dt.astimezone(timezone.utc).timestamp() * 1000)
    return f'W/"{int(version_number or 0)}-{ms}"'


def odata_payload(results: list[dict[str, Any]], inlinecount: int | None = None) -> dict[str, Any]:
    payload: dict[str, Any] = {"d": {"results": results}}
    if inlinecount is not None:
        payload["d"]["__count"] = str(inlinecount)
    return payload


def odata_error_response(status_code: int, code: str, message: str, details: list[dict[str, str]] | None = None) -> JSONResponse:
    tx = uuid.uuid4().hex
    now = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
    body = {
        "error": {
            "code": code,
            "message": {"lang": "en", "value": message},
            "innererror": {
                "transactionid": tx,
                "timestamp": now,
                "details": details or [],
            },
        }
    }
    return JSONResponse(status_code=status_code, content=body)
