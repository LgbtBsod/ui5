from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Any

from fastapi.responses import JSONResponse

from .odata_datetime import to_odata_date_ms
from .odata_etag import etag_for_datetime

SERVICE_ROOT = "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV"
ODATA_NS = "Z_EHS_PRODUCTION_CONTROL_CKLT_SRV"


def format_datetime(value: datetime | None) -> str | None:
    if value is None:
        return None
    if isinstance(value, str):
        s_value = value.strip()
        if not s_value:
            return None
        value = datetime.fromisoformat(s_value.replace("Z", "+00:00"))
    return to_odata_date_ms(value)


def format_entity_etag(changed_on: datetime | None, version_number: int | None = None) -> str | None:
    if changed_on is None:
        return None
    return etag_for_datetime(changed_on, version_number)


def odata_entity(payload: dict[str, Any]) -> dict[str, Any]:
    return {"d": payload}


def odata_payload(results: list[dict[str, Any]], inlinecount: int | None = None) -> dict[str, Any]:
    payload: dict[str, Any] = {"d": {"results": results}}
    if inlinecount is not None:
        payload["d"]["__count"] = str(inlinecount)
    return payload


def build_odata_error_payload(code: str, message: str, details: list[dict[str, str]] | None = None) -> dict[str, Any]:
    tx = uuid.uuid4().hex
    now = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
    return {
        "error": {
            "code": code,
            "message": {"lang": "en", "value": message},
            "innererror": {
                "transactionid": tx,
                "timestamp": now,
                "errordetails": details or [],
            },
        }
    }


def odata_error_response(status_code: int, code: str, message: str, details: list[dict[str, str]] | None = None) -> JSONResponse:
    return JSONResponse(
        status_code=status_code,
        content=build_odata_error_payload(code=code, message=message, details=details),
    )
