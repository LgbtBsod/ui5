from __future__ import annotations

from datetime import datetime, timezone


def etag_for_datetime(dt: datetime) -> str:
    val = dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
    return f'W/"{int(val.timestamp() * 1000)}"'


def format_etag(timestamp: datetime | None) -> str | None:
    """Backward-compatible alias: returns OData-standard W/"…" ETag or None."""
    if timestamp is None:
        return None
    return etag_for_datetime(timestamp)


def validate_if_match(if_match: str | None, current_etag: str) -> bool:
    if not if_match or if_match == "*":
        return True
    return if_match == current_etag
