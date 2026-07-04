from __future__ import annotations

from datetime import datetime, timezone


def etag_for_datetime(dt: datetime, version_number: int | None = None) -> str:
    """Build a weak ETag from a changed-on timestamp.

    Millisecond-truncated wall-clock time alone collides: two writes inside the same
    millisecond produce an identical ETag, silently defeating If-Match optimistic
    concurrency (a client's stale copy would look "current" to the server). Folding in
    version_number (already bumped atomically on every mutation - see
    ChecklistRoot.version_number) makes the ETag change on every write regardless of
    clock resolution; timestamp is kept alongside it for readability/debugging.
    """
    val = dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
    ms = int(val.timestamp() * 1000)
    if version_number is not None:
        return f'W/"{ms}-{int(version_number)}"'
    return f'W/"{ms}"'


def validate_if_match(if_match: str | None, current_etag: str) -> bool:
    # Only a genuinely absent header or the explicit "*" wildcard skip the check.
    # An empty string is neither - previously `not if_match` treated "" the same as
    # "no header", silently bypassing optimistic concurrency for a malformed request.
    if if_match is None or if_match == "*":
        return True
    return if_match == current_etag
