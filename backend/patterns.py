"""Central registry of compiled regex patterns.

This module follows the Single Responsibility Principle by centralizing
all regex pattern definitions to eliminate duplication across modules.
All patterns are pre-compiled at module load time for performance.

Usage:
    from .patterns import Patterns
    
    # Use pre-compiled patterns
    match = Patterns.SINGLE_KEY_RE.match(url_path)
    
    # Or use helper methods
    entity, key = Patterns.parse_single_key(url_path)
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Optional, Tuple, Match, Pattern


@dataclass(frozen=True)
class EntityKeyMatch:
    """Represents a parsed entity key match result.
    
    Attributes:
        entity_set: Name of the entity set
        key_parts: Dictionary of key property names and values
        is_valid: Whether the match was successful
    """
    entity_set: Optional[str]
    key_parts: Optional[dict]
    is_valid: bool
    
    @classmethod
    def success(cls, entity_set: str, key_parts: dict) -> EntityKeyMatch:
        """Create a successful match result."""
        return cls(entity_set=entity_set, key_parts=key_parts, is_valid=True)
    
    @classmethod
    def failure(cls) -> EntityKeyMatch:
        """Create a failure match result."""
        return cls(entity_set=None, key_parts=None, is_valid=False)


class Patterns:
    """Central registry of compiled regex patterns for OData URL parsing.
    
    This class provides a single source of truth for all regex patterns
    used throughout the backend, following the DRY principle.
    
    Categories:
        - Entity Key Patterns: For parsing entity URLs with keys
        - Navigation Patterns: For navigation properties
        - Filter Patterns: For $filter query parsing
        - Format Patterns: For OData format conversion
        - Batch Patterns: For batch request parsing
    """
    
    # =========================================================================
    # Entity Key Patterns
    # =========================================================================
    
    #: Single key entity: EntityName('key_value')
    SINGLE_KEY_RE: Pattern[str] = re.compile(r"^(\w+)\('([^']*)'\)$")
    
    #: Composite key entity: EntityName(RootId='val',ItemId='val')
    COMPOSITE_KEY_RE: Pattern[str] = re.compile(
        r"^(\w+)\(RootId='([^']*)',(ItemId|BarrierId)='([^']*)'\)$"
    )
    
    #: CheckRoots function import with GUIDs
    CHECKROOT_KEY_RE: Pattern[str] = re.compile(
        r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)$"
    )
    
    # =========================================================================
    # Navigation Property Patterns
    # =========================================================================
    
    #: Navigation to collection (to_Basic, to_Checks, to_Barriers)
    NAV_COLLECTION_RE: Pattern[str] = re.compile(
        r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/(to_Basic|to_Checks|to_Barriers)$"
    )
    
    #: Navigation for creating child entities
    NAV_CREATE_RE: Pattern[str] = re.compile(
        r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/(to_Checks|to_Barriers)$"
    )
    
    #: Navigation to DraftAdministrativeData
    NAV_DRAFT_ADMIN_RE: Pattern[str] = re.compile(
        r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/DraftAdministrativeData$"
    )
    
    #: Navigation to SiblingEntity
    NAV_SIBLING_RE: Pattern[str] = re.compile(
        r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/SiblingEntity$"
    )
    
    #: Count endpoint: EntitySet/$count
    COUNT_RE: Pattern[str] = re.compile(r"^(\w+)/\$count$")
    
    #: Simple entity set list: EntitySet
    ENTITY_SET_RE: Pattern[str] = re.compile(r"^(\w+)$")
    
    # =========================================================================
    # Filter Expression Patterns
    # =========================================================================
    
    #: location_name_insub('value')
    LOCATION_INSUB_RE: Pattern[str] = re.compile(r"location_name_insub\('([^']*)'\)")
    
    #: substringof('value', field) - case insensitive
    SUBSTRING_OF_RE: Pattern[str] = re.compile(
        r"substringof\('([^']*)'\s*,\s*(\w+)\)", re.I
    )
    
    #: tolower(field) eq 'value' - case insensitive
    TOLOWER_EQ_RE: Pattern[str] = re.compile(
        r"tolower\((\w+)\)\s+eq\s+'([^']*)'", re.I
    )
    
    #: field le value (less than or equal) - case insensitive
    FIELD_LE_RE: Pattern[str] = re.compile(
        r"(\w+)\s+le\s+(datetime'[^']+'|/Date\(\d+\)/|'[^']*')", re.I
    )
    
    #: field ge value (greater than or equal) - case insensitive
    FIELD_GE_RE: Pattern[str] = re.compile(
        r"(\w+)\s+ge\s+(datetime'[^']+'|/Date\(\d+\)/|'[^']*')", re.I
    )
    
    #: PkLevels eq 'value' - case insensitive
    PK_LEVELS_EQ_RE: Pattern[str] = re.compile(r"(PkLevels)\s+eq\s+'([^']*)'", re.I)
    
    #: field eq true/false - case insensitive
    FIELD_BOOL_EQ_RE: Pattern[str] = re.compile(r"([\w/]+)\s+eq\s+(true|false)", re.I)
    
    #: field ne 'value' - case insensitive
    FIELD_NE_RE: Pattern[str] = re.compile(r"([\w/]+)\s+ne\s+'([^']*)'", re.I)
    
    #: field eq 'value' (generic string equality)
    FIELD_STR_EQ_RE: Pattern[str] = re.compile(r"([\w/]+)\s+eq\s+'([^']*)'")
    
    #: field eq null - case insensitive
    FIELD_NULL_EQ_RE: Pattern[str] = re.compile(r"([\w/]+)\s+eq\s+null", re.I)
    
    #: with_sub eq 'true' - case insensitive
    WITH_SUB_RE: Pattern[str] = re.compile(r"with_sub\s+eq\s+'?true'?", re.I)
    
    # =========================================================================
    # OData Format Patterns
    # =========================================================================
    
    #: /Date(timestamp)/ format
    ODATA_DATE_RE: Pattern[str] = re.compile(r"^/Date\((\d+)\)/$")
    
    #: datetime'value' format - case insensitive
    ODATA_DATETIME_RE: Pattern[str] = re.compile(r"^datetime'([^']+)'$", re.I)
    
    #: Quoted string 'value'
    QUOTED_STRING_RE: Pattern[str] = re.compile(r"^'([^']+)'$")
    
    # =========================================================================
    # Batch Request Patterns
    # =========================================================================
    
    #: HTTP request line in batch part
    HTTP_REQUEST_LINE_RE: Pattern[str] = re.compile(
        r"^(GET|POST|PUT|PATCH|DELETE|MERGE)\s+(\S+)\s+HTTP", re.I
    )
    
    #: Content-Type header with multipart boundary
    MULTIPART_BOUNDARY_RE: Pattern[str] = re.compile(
        r"content-type:\s*multipart/mixed;\s*boundary=([^\s;]+)", re.I
    )
    
    #: Boundary parameter extraction
    BOUNDARY_PARAM_RE: Pattern[str] = re.compile(r"boundary=([^\s;]+)")
    
    # =========================================================================
    # CORS/Origin Patterns
    # =========================================================================
    
    #: Localhost origins
    LOCAL_ORIGIN_RE: Pattern[str] = re.compile(
        r"^https?://(localhost|127\.0\.0\.1)(:\d+)?$", re.I
    )
    
    #: Preview environment origins
    PREVIEW_ORIGIN_RE: Pattern[str] = re.compile(
        r"^https://[a-z0-9-]+\.space-z\.ai$", re.I
    )
    
    # =========================================================================
    # Helper Methods
    # =========================================================================
    
    @staticmethod
    def parse_single_key(url_path: str) -> EntityKeyMatch:
        """Parse a single-key entity URL.
        
        Args:
            url_path: URL path like \"EntitySet('key')\"
            
        Returns:
            EntityKeyMatch with parsed entity set and key
        """
        match = Patterns.SINGLE_KEY_RE.match(url_path)
        if not match:
            return EntityKeyMatch.failure()
        
        from urllib.parse import unquote
        entity_set, key_value = match.groups()
        return EntityKeyMatch.success(
            entity_set=entity_set,
            key_parts={"key": unquote(key_value)}
        )
    
    @staticmethod
    def parse_composite_key(url_path: str) -> EntityKeyMatch:
        """Parse a composite-key entity URL.
        
        Args:
            url_path: URL path like \"EntitySet(RootId='val',ItemId='val')\"
            
        Returns:
            EntityKeyMatch with parsed entity set and key parts
        """
        match = Patterns.COMPOSITE_KEY_RE.match(url_path)
        if not match:
            return EntityKeyMatch.failure()
        
        from urllib.parse import unquote
        entity_set, root_id, id_prop, id_val = match.groups()
        return EntityKeyMatch.success(
            entity_set=entity_set,
            key_parts={
                "RootId": unquote(root_id),
                id_prop: unquote(id_val)
            }
        )
    
    @staticmethod
    def parse_checkroots_key(url_path: str) -> EntityKeyMatch:
        """Parse CheckRoots function import URL.
        
        Args:
            url_path: URL path like \"CheckRoots(ActiveUUID=guid'...',DraftUUID=guid'...')\"
            
        Returns:
            EntityKeyMatch with parsed UUIDs
        """
        match = Patterns.CHECKROOT_KEY_RE.match(url_path)
        if not match:
            return EntityKeyMatch.failure()
        
        active_uuid, draft_uuid = match.groups()
        return EntityKeyMatch.success(
            entity_set="CheckRoots",
            key_parts={"ActiveUUID": active_uuid, "DraftUUID": draft_uuid}
        )
    
    @staticmethod
    def parse_count_endpoint(url_path: str) -> Optional[str]:
        """Extract entity set name from $count endpoint.
        
        Args:
            url_path: URL path like \"EntitySet/$count\"
            
        Returns:
            Entity set name or None if no match
        """
        match = Patterns.COUNT_RE.match(url_path)
        return match.group(1) if match else None
    
    @staticmethod
    def parse_entity_set(url_path: str) -> Optional[str]:
        """Extract entity set name from simple list endpoint.
        
        Args:
            url_path: URL path like \"EntitySet\"
            
        Returns:
            Entity set name or None if no match
        """
        match = Patterns.ENTITY_SET_RE.match(url_path)
        return match.group(1) if match else None


# Export commonly used patterns for backward compatibility
SINGLE_KEY_RE = Patterns.SINGLE_KEY_RE
COMPOSITE_KEY_RE = Patterns.COMPOSITE_KEY_RE
CHECKROOT_KEY_RE = Patterns.CHECKROOT_KEY_RE
NAV_COLLECTION_RE = Patterns.NAV_COLLECTION_RE
NAV_CREATE_RE = Patterns.NAV_CREATE_RE
NAV_DRAFT_ADMIN_RE = Patterns.NAV_DRAFT_ADMIN_RE
NAV_SIBLING_RE = Patterns.NAV_SIBLING_RE
