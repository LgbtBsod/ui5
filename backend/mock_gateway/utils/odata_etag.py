from __future__ import annotations

from datetime import datetime, timezone


def etag_for_datetime(dt: datetime) -> str:
    val = dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
    return f'W/"{int(val.timestamp() * 1000)}"'


def validate_if_match(if_match: str | None, current_etag: str) -> bool:
    if not if_match or if_match == "*":
        return True
    return if_match == current_etag
