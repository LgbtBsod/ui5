"""
Type definitions for gateway API: DTOs, schemas, request/response contracts.
Centralized type safety across serializers, validators, operations.
"""
from typing import Any, Optional, Dict, List, Tuple
from dataclasses import dataclass
from datetime import datetime
from pydantic import BaseModel, Field


# ============================================================================
# OData Response Types
# ============================================================================

@dataclass
class ODataMetadata:
    """OData __metadata structure for entity responses."""
    type: str = Field(description="Entity type URI (e.g., Z_EHS_PRODUCTION_CONTROL_CKLT_SRV.ChecklistRoot)")
    uri: str = Field(description="Entity URI for navigation")


@dataclass
class ODataError:
    """OData v2 error response structure."""
    code: str = Field(description="Error code (e.g., VALIDATION_ERROR)")
    message: str = Field(description="Human-readable error message")
    severity: str = Field(default="error", description="error|warning|info")
    details: Optional[List[Dict[str, str]]] = Field(default=None, description="Additional error context")


class ODataEntity(BaseModel):
    """Generic OData entity wrapper with metadata."""
    __metadata: Optional[Dict[str, str]] = Field(alias="__metadata")

    class Config:
        allow_population_by_field_name = True


# ============================================================================
# Checklist DTOs
# ============================================================================

class ChecklistRootDTO(BaseModel):
    """Checklist root entity data transfer object."""
    Key: str = Field(description="32-char hex UUID")
    RequestId: str = Field(description="Business checklist ID")
    Status: str = Field(description="DRAFT|REGISTERED|CLOSED")
    Lpc: str = Field(description="Location of use code")
    LpcText: str = Field(description="Localized LPC text")
    DateCheck: str = Field(description="Check date (YYYY-MM-DD)")
    Equipment: str = Field(description="Equipment identifier")
    Bukrs: str = Field(description="Company code")
    ObserverFullname: str = Field(description="Observer name")
    ChangedOn: str = Field(description="ISO 8601 timestamp")
    CreatedOn: str = Field(description="ISO 8601 timestamp")
    VersionNumber: int = Field(description="Optimistic lock version")


class ChecklistCheckDTO(BaseModel):
    """Checklist check (item) data transfer object."""
    Key: str = Field(description="32-char hex UUID")
    ParentKey: str = Field(description="Parent root key")
    ChecksNum: int = Field(description="Sequential position")
    Text: str = Field(description="Check text")
    Result: bool = Field(description="Pass/Fail result")
    Comment: str = Field(default="", description="Optional comment")


class ChecklistBarrierDTO(BaseModel):
    """Checklist barrier data transfer object."""
    Key: str = Field(description="32-char hex UUID")
    ParentKey: str = Field(description="Parent root key")
    BarriersNum: int = Field(description="Sequential position")
    Description: str = Field(description="Barrier description")
    Result: bool = Field(description="Active/Inactive result")


class AttachmentDTO(BaseModel):
    """Attachment entity data transfer object."""
    AttachmentKey: str = Field(description="32-char hex UUID")
    DB_KEY: str = Field(description="Parent root key")
    FileName: str = Field(description="File name with extension")
    MimeType: str = Field(description="MIME type (e.g., application/pdf)")
    FileSize: int = Field(description="File size in bytes")
    Description: str = Field(default="", description="Optional description")
    CategoryKey: str = Field(default="GEN", description="Attachment category")


# ============================================================================
# Request/Response Types
# ============================================================================

@dataclass
class PaginationParams:
    """OData $top, $skip, $inlinecount parameters."""
    top: int
    skip: int
    inlinecount: Optional[str]

    @property
    def offset(self) -> int:
        return self.skip

    @property
    def limit(self) -> int:
        return self.top


@dataclass
class FilterParams:
    """OData $filter, $orderby, $select, $expand parameters."""
    filter: Optional[str]
    orderby: Optional[str]
    select: Optional[str]
    expand: Optional[str]


@dataclass
class SavePayload:
    """Unified save request payload structure."""
    DB_KEY: str = Field(description="Root entity key")
    Status: Optional[str] = None
    Changes: Optional[List[Dict[str, Any]]] = None
    Attachments: Optional[List[AttachmentDTO]] = None
    ClientAggChangedOn: Optional[str] = None
    SessionGuid: Optional[str] = None


# ============================================================================
# Operation Result Types
# ============================================================================

@dataclass
class OperationResult:
    """Generic operation result with typed payload."""
    success: bool
    data: Optional[Any] = None
    error: Optional[ODataError] = None
    status_code: int = 200


@dataclass
class BulkOperationResult:
    """Result of batch/bulk operations."""
    total: int
    succeeded: int
    failed: int
    errors: List[ODataError]


# ============================================================================
# Function Signatures
# ============================================================================

# Serializers: raw model → OData dict
SerializerFn = callable  # (model: Any, db: Optional[Session]) -> Dict[str, Any]

# Validators: raw input → normalized dict
ValidatorFn = callable  # (payload: Dict[str, Any], db: Optional[Session]) -> Dict[str, Any]

# Operators: perform business logic, return result or error
OperatorFn = callable  # (models, db: Session, ...) -> OperationResult

# Filters: predicate matching
FilterFn = callable  # (row: Any, expr: str) -> bool
