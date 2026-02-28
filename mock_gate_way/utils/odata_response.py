from __future__ import annotations

import uuid
from datetime import datetime, timezone
from typing import Any

from fastapi import HTTPException


def odata_entity(payload: dict[str, Any]) -> dict[str, Any]:
    return {"d": payload}


def odata_collection(results: list[dict[str, Any]], inlinecount: int | None = None) -> dict[str, Any]:
    body: dict[str, Any] = {"d": {"results": results}}
    if inlinecount is not None:
        body["d"]["__count"] = str(inlinecount)
    return body


def odata_error(code: str, message: str, status: int, details: list[dict[str, str]] | None = None) -> None:
    tx = uuid.uuid4().hex
    now = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
    raise HTTPException(
        status_code=status,
        detail={
            "error": {
                "code": code,
                "message": {"lang": "en", "value": message},
                "innererror": {
                    "transactionid": tx,
                    "timestamp": now,
                    "details": details or [],
                },
            }
        },
    )
