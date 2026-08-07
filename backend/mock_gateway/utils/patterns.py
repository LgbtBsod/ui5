"""Centralized regular expression patterns for the application.

This module follows the DRY (Don't Repeat Yourself) principle by consolidating
all regex patterns in a single location. This prevents duplication across modules
and makes pattern maintenance easier.

Patterns are organized by domain:
- OData parsing patterns
- Key resolution patterns  
- Filter/tokenization patterns
- Validation patterns
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Pattern


@dataclass(frozen=True)
class PatternDefinition:
    """Immutable definition of a regex pattern with metadata."""

    name: str
    pattern: Pattern[str]
    description: str
    flags: int = 0


# =============================================================================
# OData Batch Parsing Patterns
# =============================================================================

#: Extract boundary from Content-Type header (supports quoted and unquoted)
#: Example: multipart/mixed; boundary=batch_123 or boundary="batch_123"
ODATA_BOUNDARY_PATTERN = re.compile(
    r"boundary=(?:\"([^\"]+)\"|([^\s;]+))",
    re.IGNORECASE,
)

#: Split multipart body by boundary marker (RFC 2046 compliant)
#: Matches boundary at start of line or beginning of string
def make_odata_multipart_marker(boundary: str) -> Pattern[str]:
    """Create a pattern to split multipart body by boundary.
    
    Args:
        boundary: The boundary string to escape and use in pattern
        
    Returns:
        Compiled regex pattern for splitting multipart content
    """
    return re.compile(rf"(?:\A|\n)--{re.escape(boundary)}")

#: Parse HTTP request line in batch part
#: Matches: METHOD path [HTTP/version]
#: Examples: "GET /ChecklistSet", "PATCH ChecklistSet('id') HTTP/1.1"
ODATA_BATCH_REQUEST_LINE_PATTERN = re.compile(
    r"^([A-Z]+)\s+(.+?)\s*(?:HTTP/\d(?:\.\d+)?)?$",
    re.IGNORECASE,
)


# =============================================================================
# Key Resolution Patterns
# =============================================================================

#: Parse compound draft key: ActiveUUID='...',DraftUUID='...'
#: Used in OData URLs for draft-enabled entities
DRAFT_KEY_PATTERN = re.compile(r"^ActiveUUID='([^']*)',DraftUUID='([^']*)'$")

#: Extract binary literal from OData format: binary'HEXVALUE'
#: Used for RAW16/Edm.Binary key types
BINARY_LITERAL_PATTERN = re.compile(r"^binary'(.*)'$", re.IGNORECASE)

#: Validate hex GUID format (32 hex characters, with or without dashes)
HEX_GUID_PATTERN = re.compile(r"^[0-9a-fA-F]{8}-?[0-9a-fA-F]{4}-?[0-9a-fA-F]{4}-?[0-9a-fA-F]{4}-?[0-9a-fA-F]{12}$")

#: Match pure hex string (no dashes) - 32 characters
HEX32_PATTERN = re.compile(r"^[0-9a-fA-F]{32}$")


# =============================================================================
# OData Filter Tokenization Patterns
# =============================================================================

#: Tokenize OData $filter expressions
#: Matches: functions, datetime literals, strings, operators, identifiers, numbers
#: This is the single source of truth for $filter grammar
ODATA_FILTER_TOKEN_PATTERN = re.compile(
    r"substringof\(|contains\(|startswith\(|datetime'[^']*'|/Date\([^)]*\)/|"
    r"'[^']*'|\(|\)|,|"
    r"\b(?:and|or|not|eq|ne|gt|ge|lt|le|true|false)\b|"
    r"[A-Za-z_][A-Za-z0-9_]*|"
    r"-?\d+(?:\.\d+)?",
    re.IGNORECASE,
)

#: Set of comparison operator tokens
ODATA_COMPARISON_OPS = frozenset({"eq", "ne", "gt", "lt", "ge", "le"})

#: Set of function tokens that need special handling
ODATA_FUNCTION_TOKENS = frozenset({"substringof(", "contains(", "startswith("})


# =============================================================================
# Content-Type Patterns
# =============================================================================

#: Check if Content-Type is JSON (with optional charset)
JSON_CONTENT_TYPE_PATTERN = re.compile(r"^application/json(;.*)?$", re.IGNORECASE)

#: Extract charset from Content-Type header
CHARSET_PATTERN = re.compile(r"charset=([^\s;]+)", re.IGNORECASE)


# =============================================================================
# Upload/Attachment Patterns
# =============================================================================

#: Validate filename - allow alphanumeric, dashes, underscores, dots
FILENAME_PATTERN = re.compile(r"^[a-zA-Z0-9_\-.]+$")

#: Detect base64 encoded content in payload
BASE64_DATA_URI_PATTERN = re.compile(r"^data:[^;]+;base64,")


# =============================================================================
# Pattern Registry (for documentation and testing)
# =============================================================================

PATTERN_REGISTRY: tuple[PatternDefinition, ...] = (
    PatternDefinition(
        name="ODATA_BOUNDARY",
        pattern=ODATA_BOUNDARY_PATTERN,
        description="Extract boundary from Content-Type header",
    ),
    PatternDefinition(
        name="ODATA_BATCH_REQUEST_LINE",
        pattern=ODATA_BATCH_REQUEST_LINE_PATTERN,
        description="Parse HTTP request line in OData batch",
    ),
    PatternDefinition(
        name="DRAFT_KEY",
        pattern=DRAFT_KEY_PATTERN,
        description="Parse compound draft key format",
    ),
    PatternDefinition(
        name="BINARY_LITERAL",
        pattern=BINARY_LITERAL_PATTERN,
        description="Extract binary literal from OData format",
    ),
    PatternDefinition(
        name="HEX_GUID",
        pattern=HEX_GUID_PATTERN,
        description="Validate hex GUID format",
    ),
    PatternDefinition(
        name="HEX32",
        pattern=HEX32_PATTERN,
        description="Match pure 32-char hex string",
    ),
    PatternDefinition(
        name="ODATA_FILTER_TOKEN",
        pattern=ODATA_FILTER_TOKEN_PATTERN,
        description="Tokenize OData $filter expressions",
    ),
    PatternDefinition(
        name="JSON_CONTENT_TYPE",
        pattern=JSON_CONTENT_TYPE_PATTERN,
        description="Check if Content-Type is JSON",
    ),
    PatternDefinition(
        name="CHARSET",
        pattern=CHARSET_PATTERN,
        description="Extract charset from Content-Type",
    ),
    PatternDefinition(
        name="FILENAME",
        pattern=FILENAME_PATTERN,
        description="Validate filename format",
    ),
    PatternDefinition(
        name="BASE64_DATA_URI",
        pattern=BASE64_DATA_URI_PATTERN,
        description="Detect base64 data URI",
    ),
)


def get_pattern(name: str) -> Pattern[str] | None:
    """Retrieve a pattern by name from the registry.
    
    Args:
        name: Pattern name (case-insensitive)
        
    Returns:
        Compiled regex pattern or None if not found
        
    Example:
        >>> pattern = get_pattern("hex_guid")
        >>> pattern.match("550e8400-e29b-41d4-a716-446655440000")
        <re.Match object>
    """
    name_upper = name.upper()
    for definition in PATTERN_REGISTRY:
        if definition.name == name_upper:
            return definition.pattern
    return None


def list_patterns() -> list[str]:
    """List all available pattern names.
    
    Returns:
        List of pattern names
        
    Example:
        >>> list_patterns()
        ['ODATA_BOUNDARY', 'ODATA_BATCH_REQUEST_LINE', ...]
    """
    return [definition.name for definition in PATTERN_REGISTRY]
