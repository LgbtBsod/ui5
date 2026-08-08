"""Type-safe constants and enumerations for the Redux OData mock server.

This module replaces magic strings/numbers with typed enums and constants,
following Python best practices (PEP 484, PEP 593).
"""
from __future__ import annotations

from enum import Enum
from typing import Final


class ErrorCode(str, Enum):
    """Standardized error codes for OData responses."""
    
    VALIDATION_ERROR = "VALIDATION_ERROR"
    DRAFT_LOCKED = "DRAFT_LOCKED"
    NOT_FOUND = "NOT_FOUND"
    PRECONDITION_FAILED = "PRECONDITION_FAILED"
    UNKNOWN_ROUTE = "UNKNOWN_ROUTE"
    UNKNOWN_ENTITY_SET = "UNKNOWN_ENTITY_SET"


class ErrorSeverity(str, Enum):
    """Error severity levels following SAP Gateway convention."""
    
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"
    SUCCESS = "success"


class DraftOperation(str, Enum):
    """Draft lifecycle operations."""
    
    PREPARATION = "CheckRootPreparationAction"
    ACTIVATION = "CheckRootActivationAction"
    DISCARD = "CheckRootDiscardAction"


class NavigationProperty(str, Enum):
    """OData navigation properties."""
    
    TO_BASIC = "to_Basic"
    TO_CHECKS = "to_Checks"
    TO_BARRIERS = "to_Barriers"
    DRAFT_ADMIN = "DraftAdministrativeData"
    SIBLING = "SiblingEntity"


class EntitySet(str, Enum):
    """Entity set names - single source of truth."""
    
    CHECK_ROOTS = "CheckRoots"
    CHECK_BASICS = "CheckBasics"
    CHECK_ITEMS = "CheckItems"
    BARRIERS = "Barriers"
    DRAFT_ADMIN_DATA = "DraftAdministrativeData"


# Criticality levels (UI5 semantic colors)
class CriticalityLevel(int, Enum):
    """Criticality indicators for UI5 semantic coloring."""
    
    NEGATIVE = 1  # Red
    NEUTRAL = 2   # Yellow
    POSITIVE = 3  # Green
    NONE = 0      # No indicator


# Field control values (sap.ui.comp.smartfield.FieldControl)
class FieldControl(int, Enum):
    """Field control states for dynamic editability."""
    
    OPTIONAL = 3      # Optional/optional
    MANDATORY = 7     # Mandatory/required


# HTTP status codes as constants (avoiding enum inheritance issues)
class HttpStatus:
    """HTTP status codes including SAP Gateway conventions."""
    
    OK = 200
    CREATED = 201
    NO_CONTENT = 204
    BAD_REQUEST = 400
    NOT_FOUND = 404
    PRECONDITION_FAILED = 412
    BAD_REQUEST_DRAFT_LOCKED = 409  # Conflict - draft held by another user


# Result codes
RESULT_CODE_SATISFACTORY: Final[str] = "X"
RESULT_CODE_UNSATISFACTORY: Final[str] = ""

# Zero GUID placeholder for draft protocol
ZERO_GUID: Final[str] = "00000000-0000-0000-0000-000000000000"

# Default mock user identity
DEFAULT_MOCK_USER: Final[str] = "MOCK_USER"

# CSRF token placeholder
CSRF_TOKEN_DEFAULT: Final[str] = "dummy-csrf-token"

# Required field validation
REQUIRED_CHILD_FIELD: Final[str] = "Result"
REQUIRED_CHILD_FIELD_LABEL: Final[str] = "Результат"

# ETag sentinel timestamp (1999-01-01 UTC in milliseconds)
ETAG_SENTINEL_MS: Final[int] = 883612800000
