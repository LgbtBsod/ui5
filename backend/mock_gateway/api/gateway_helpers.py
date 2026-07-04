"""
Gateway helper utilities: SOLID/DRY refactoring of common patterns.
Extracts repeated logic from gateway_canonical_api.py (62 functions → consolidated helpers).
"""
from typing import Any, Optional, Dict, List, Callable
from datetime import datetime, timezone
from sqlalchemy.orm import Session
import re
from models import DictionaryItem


# ============================================================================
# Payload/Dict Extraction Helpers (DRY: consolidate 5+ similar functions)
# ============================================================================

class PayloadExtractor:
    """Single Responsibility: Extract & coerce values from dict payloads."""

    @staticmethod
    def text(payload: Dict | None, *keys: str, default: str = "") -> str:
        """Extract first non-empty string value with fallback chain."""
        if not isinstance(payload, dict):
            return default
        for key in keys:
            value = payload.get(key, "")
            if value and isinstance(value, str):
                return str(value).strip() or default
        return default

    @staticmethod
    def int(payload: Dict | None, *keys: str, default: int = 0) -> int:
        """Extract and coerce to int with fallback."""
        if not isinstance(payload, dict):
            return default
        for key in keys:
            value = payload.get(key)
            if value is not None:
                try:
                    return int(value)
                except (TypeError, ValueError):
                    pass
        return default

    @staticmethod
    def bool(payload: Dict | None, *keys: str, default: bool = True) -> bool:
        """Extract and coerce to bool with fallback."""
        if not isinstance(payload, dict):
            return default
        for key in keys:
            value = payload.get(key)
            if value is not None:
                return bool(value)
        return default

    @staticmethod
    def any(payload: Dict | None, *keys: str, default: Any = None) -> Any:
        """Extract first present value (any type)."""
        if not isinstance(payload, dict):
            return default
        for key in keys:
            if key in payload and payload[key] is not None:
                return payload[key]
        return default


# ============================================================================
# Date Parsing (DRY: consolidate _date_ymd_from_any, _date_ms_from_any)
# ============================================================================

class DateParser:
    """Single Responsibility: Parse various date formats to standard representations."""

    @staticmethod
    def to_ymd(value: Any) -> str:
        """Extract YYYY-MM-DD from various formats (SAP /Date(...), ISO8601, etc.)."""
        if value is None:
            return ""

        raw = str(value)

        # SAP /Date(1234567890)/ format
        if raw.startswith("/Date(") and raw.endswith(")/"):
            try:
                ms = int(raw[6:-2].split("+")[0].split("-")[0])
                dt = datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
                return dt.strftime("%Y-%m-%d")
            except (ValueError, IndexError):
                return ""

        # datetime'2024-01-15T00:00:00' format
        if raw.lower().startswith("datetime'") and raw.endswith("'"):
            raw = raw[9:-1]

        # Extract YYYY-MM-DD part
        return raw.split("T", 1)[0][:10] if raw else ""

    @staticmethod
    def to_ms(value: Any) -> int:
        """Extract milliseconds from various formats."""
        if value is None:
            return 0

        raw = str(value)

        # SAP /Date(1234567890)/ format
        if raw.startswith("/Date(") and raw.endswith(")/"):
            body = raw[6:-2]
            sign_pos = max(body.find("+", 1), body.find("-", 1))
            ms_part = body if sign_pos < 0 else body[:sign_pos]
            try:
                return int(ms_part)
            except ValueError:
                return 0

        # datetime'2024-01-15T00:00:00' format
        if raw.lower().startswith("datetime'") and raw.endswith("'"):
            raw = raw[9:-1]

        # ISO8601 format
        try:
            dt = datetime.fromisoformat(raw.replace("Z", "+00:00"))
            dt = dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
            return int(dt.astimezone(timezone.utc).timestamp() * 1000)
        except (ValueError, TypeError):
            return 0


# ============================================================================
# Boundary Key Resolution (DRY: consolidate _entity_key, _boundary_root_key, etc.)
# ============================================================================

class BoundaryResolver:
    """Single Responsibility: Resolve entity keys from various payload formats."""

    @staticmethod
    def resolve_key(key_expr: str) -> str:
        """Extract entity key from OData expression (Key=..., DB_KEY=..., etc.)."""
        cleaned = str(key_expr or "").strip()

        # Remove OData key= prefix
        for prefix in ("Key=", "DB_KEY=", "PARENT_KEY=", "AttachmentKey=", "FolderKey="):
            if cleaned.startswith(prefix):
                cleaned = cleaned.split("=", 1)[1].strip()

        # Remove quotes
        cleaned = cleaned.strip("'")
        return cleaned

    @staticmethod
    def resolve_root_key(payload: Dict | None = None, *candidates: str) -> str:
        """Find root entity key from payload with fallback candidates."""
        if payload and isinstance(payload, dict):
            for candidate in [*candidates, payload.get("DB_KEY"), payload.get("db_key")]:
                if candidate:
                    raw = str(candidate).strip()
                    if raw and raw != "__CREATE":
                        return raw

        return ""

    @staticmethod
    def resolve_parent_key(payload: Dict | None = None, *candidates: str) -> str:
        """Find parent entity key from payload with fallback candidates."""
        if payload and isinstance(payload, dict):
            for candidate in [*candidates, payload.get("PARENT_KEY"), payload.get("parent_key"), payload.get("ParentKey")]:
                if candidate:
                    raw = str(candidate).strip()
                    if raw:
                        return raw

        return ""


# ============================================================================
# OData Serialization Base (DRY: consolidate 10+ _to_* functions)
# ============================================================================

class ODataSerializer:
    """Single Responsibility: Base OData entity serialization (Template Method Pattern)."""

    @staticmethod
    def build_metadata(entity_type: str, entity_set: str, key_value: str) -> dict:
        """Build OData __metadata structure (reusable for all entity types)."""
        from utils.odata import SERVICE_ROOT
        return {
            "type": f"{SERVICE_ROOT.strip('/')}.{entity_type}",
            "uri": f"{SERVICE_ROOT}{entity_set}('{key_value}')",
        }

    @staticmethod
    def build_entity(
        entity_type: str,
        entity_set: str,
        key_value: str,
        data: Dict[str, Any],
    ) -> Dict[str, Any]:
        """Generic entity builder: inject __metadata + data."""
        return {
            "__metadata": ODataSerializer.build_metadata(entity_type, entity_set, key_value),
            **data,
        }


# ============================================================================
# Filter & Search (DRY: consolidate matching/filtering logic)
# ============================================================================

class FilterMatcher:
    """Single Responsibility: OData filter expressions & matching."""

    @staticmethod
    def extract_eq_value(filter_expr: str, field_name: str, flags: int = re.IGNORECASE) -> Optional[str]:
        """Extract value from OData 'field eq value' expression."""
        pattern = rf"(?:{field_name}|{field_name.replace('_', '|')})\s+eq\s+(?:binary')?'?([^']+)'?"
        match = re.search(pattern, filter_expr or "", flags=flags)
        return match.group(1) if match else None

    @staticmethod
    def evaluate_segment(segment: str, condition: bool) -> bool:
        """Evaluate segment filter (ALL, SUCCESS, FAILED)."""
        normalized = str(segment or "ALL").strip().upper() or "ALL"
        if normalized == "FAILED":
            return bool(condition)
        if normalized == "SUCCESS":
            return not bool(condition)
        return True  # ALL


# ============================================================================
# Database Dictionary Lookups (DRY: cache pattern for repeated _dict_text calls)
# ============================================================================

class DictionaryCache:
    """Single Responsibility: Cache dictionary lookups (domain/key → text)."""

    def __init__(self):
        self._cache: Dict[tuple, str] = {}

    def get_text(self, db: Session, domain: str, key: str) -> str:
        """Get dictionary text with caching (per request lifetime)."""
        cache_key = (domain, key)
        if cache_key not in self._cache:
            item = db.query(DictionaryItem).filter(
                DictionaryItem.domain == domain,
                DictionaryItem.key == key,
            ).first()
            self._cache[cache_key] = item.text if item else ""
        return self._cache[cache_key]

    def clear(self) -> None:
        """Clear cache (call at end of request)."""
        self._cache.clear()


# ============================================================================
# Aggregation Functions (DRY: consolidate _rate, _export_segment_match)
# ============================================================================

class Aggregator:
    """Single Responsibility: Aggregate/summarize collection data."""

    @staticmethod
    def calculate_rate(items: List[Any], predicate: Callable[[Any], bool]) -> float:
        """Calculate success rate (matching / total)."""
        if not items:
            return 100.0
        matching = sum(1 for item in items if predicate(item))
        return round((matching / len(items)) * 100, 2)

    @staticmethod
    def count_by_predicate(items: List[Any], predicate: Callable[[Any], bool]) -> int:
        """Count items matching predicate."""
        return sum(1 for item in items if predicate(item))
