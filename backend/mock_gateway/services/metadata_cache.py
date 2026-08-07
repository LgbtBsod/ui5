from __future__ import annotations

from datetime import datetime, timezone
from threading import Lock

from services.metadata_builder import build_metadata


class MetadataCacheService:
    """Thread-safe metadata cache service with explicit state management."""
    
    def __init__(self) -> None:
        self._lock = Lock()
        self._cached_metadata: str | None = None
        self._cached_at: datetime | None = None
    
    def refresh(self) -> str:
        """Refresh metadata cache and return updated metadata.
        
        Returns:
            str: Fresh metadata string
        """
        metadata = build_metadata()
        with self._lock:
            self._cached_metadata = metadata
            self._cached_at = datetime.now(timezone.utc)
        return metadata
    
    def get(self) -> str:
        """Get cached metadata or refresh if not present.
        
        Returns:
            str: Cached or freshly generated metadata
        """
        with self._lock:
            if self._cached_metadata is not None:
                return self._cached_metadata
        return self.refresh()
    
    def refreshed_at_iso(self) -> str | None:
        """Get ISO format timestamp of last cache refresh.
        
        Returns:
            str | None: ISO timestamp or None if never refreshed
        """
        with self._lock:
            if self._cached_at is None:
                return None
            return self._cached_at.isoformat()


# Global singleton instance for backward compatibility
_metadata_cache_instance = MetadataCacheService()


def refresh_metadata() -> str:
    """Refresh metadata cache (backward compatible wrapper).
    
    Returns:
        str: Fresh metadata string
    """
    return _metadata_cache_instance.refresh()


def get_metadata() -> str:
    """Get cached metadata (backward compatible wrapper).
    
    Returns:
        str: Cached or freshly generated metadata
    """
    return _metadata_cache_instance.get()


def metadata_refreshed_at_iso() -> str | None:
    """Get ISO timestamp of last refresh (backward compatible wrapper).
    
    Returns:
        str | None: ISO timestamp or None if never refreshed
    """
    return _metadata_cache_instance.refreshed_at_iso()
