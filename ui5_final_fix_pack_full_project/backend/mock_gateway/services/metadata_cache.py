from __future__ import annotations

from datetime import datetime, timezone
from threading import Lock

from services.metadata_builder import build_metadata

_lock = Lock()
_cached_metadata: str | None = None
_cached_at: datetime | None = None


def refresh_metadata() -> str:
    global _cached_metadata, _cached_at
    metadata = build_metadata()
    with _lock:
        _cached_metadata = metadata
        _cached_at = datetime.now(timezone.utc)
    return metadata


def get_metadata() -> str:
    with _lock:
        if _cached_metadata is not None:
            return _cached_metadata
    return refresh_metadata()


def metadata_refreshed_at_iso() -> str | None:
    with _lock:
        if _cached_at is None:
            return None
        return _cached_at.isoformat()
