from __future__ import annotations

import uuid

from utils.odata_response import odata_error


def normalize_raw16_hex(value: str) -> str:
    raw = str(value or "").strip().strip("\"").strip("'")
    if raw.lower().startswith("0x"):
        odata_error("VALIDATION_ERROR", "RAW16 key must not start with 0x", 400)
    compact = raw.replace("-", "")
    try:
        uuid.UUID(hex=compact)
    except (ValueError, AttributeError):
        odata_error("VALIDATION_ERROR", "RAW16 key must be 32 hex chars", 400)
    return compact.upper()
