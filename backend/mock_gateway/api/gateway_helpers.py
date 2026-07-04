"""Boundary key resolution and OData entity-serialization base helpers."""
from typing import Any, Dict
import re
from utils.key_normalizer import hex_to_storage_key


class BoundaryResolver:
    """Single Responsibility: Resolve entity keys from various payload formats."""

    @staticmethod
    def resolve_key(key_expr: str) -> str:
        """Extract entity key from OData expression (Key=..., DB_KEY=..., etc.).

        DB_KEY is serialized to clients as a 32-char hex string with no dashes
        (see gateway_operations._hex), but ChecklistRoot.id/AttachmentEntry.id are
        stored as canonical dashed UUID strings. Without normalizing back to the
        dashed form here, every round-trip of a client-supplied DB_KEY into a
        `ChecklistRoot.id == ...` lookup would silently miss (404) even for a key
        that was just handed to that same client.
        """
        cleaned = str(key_expr or "").strip()

        # Remove OData key= prefix
        for prefix in ("Key=", "DB_KEY=", "PARENT_KEY=", "AttachmentKey=", "FolderKey="):
            if cleaned.startswith(prefix):
                cleaned = cleaned.split("=", 1)[1].strip()

        # RAW16/Edm.Binary keys may arrive as the OData binary literal binary'HEX...'
        # (SADL/CDS BINTOHEX convention referenced in README_ODATA.md); unwrap it before
        # the generic quote-strip below, which would otherwise leave a stray "binary" prefix.
        binary_literal = re.match(r"^binary'(.*)'$", cleaned, re.IGNORECASE)
        cleaned = binary_literal.group(1) if binary_literal else cleaned.strip("'")

        return hex_to_storage_key(cleaned)

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


class ODataSerializer:
    """Single Responsibility: Base OData entity serialization (Template Method Pattern)."""

    @staticmethod
    def build_metadata(entity_type: str, entity_set: str, key_value: str) -> dict:
        """Build OData __metadata structure (reusable for all entity types)."""
        from utils.odata import ODATA_NS, SERVICE_ROOT
        safe_key = str(key_value or "").replace("'", "''")
        return {
            "type": f"{ODATA_NS}.{entity_type}",
            "uri": f"{SERVICE_ROOT}/{entity_set}('{safe_key}')",
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
