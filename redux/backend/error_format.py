"""SAP Gateway OData v2 Error Format - Single Source of Truth.

Centralized error formatting following SAP Gateway convention for OData v2.
All error responses throughout the application should use these functions.

SSOT: This module is the ONLY place where error response structure is defined.
DRY: No duplication of error format logic across dispatch.py, http_server.py, etc.
SRP: Only concerned with error formatting, not business logic or HTTP handling.
"""
from __future__ import annotations

from typing import Any


def format_error_response(
    code: str,
    message_value: str,
    severity: str = "error",
    http_status: int = 400,
    lang: str = "ru",
    errordetails: list[dict[str, Any]] | None = None,
) -> tuple[int, dict[str, Any], str]:
    """Format error response in SAP Gateway OData v2 format.
    
    Args:
        code: Error code string (e.g., 'VALIDATION_ERROR', 'DRAFT_LOCKED')
        message_value: Human-readable message text
        severity: Error severity ('error', 'warning', 'info')
        http_status: HTTP status code (default 400)
        lang: Language code for message (default 'ru')
        errordetails: Optional list of detailed error objects with target binding
    
    Returns:
        Tuple of (http_status, error_dict, content_type)
        
    SAP Gateway OData v2 error format:
    {
        "error": {
            "code": "ERROR_CODE",
            "message": {"value": "Human readable message", "lang": "ru"},
            "innererror": {
                "errordetails": [
                    {
                        "code": "DETAIL_CODE",
                        "message": {"value": "Detail message"},
                        "severity": "error",
                        "target": "PropertyName"
                    }
                ]
            }
        }
    }
    """
    error_obj: dict[str, Any] = {
        "error": {
            "code": code,
            "message": {"value": message_value, "lang": lang},
        }
    }
    
    # Add errordetails if provided (for field-level validation errors)
    if errordetails:
        error_obj["error"]["innererror"] = {"errordetails": errordetails}
    
    return (http_status, error_obj, "application/json")


def format_validation_error(
    message_value: str,
    field_errors: list[tuple[str, str]],
    http_status: int = 400,
) -> tuple[int, dict[str, Any], str]:
    """Format validation error with field-level targets for UI binding.
    
    Args:
        message_value: General validation failure message
        field_errors: List of (field_name, field_label) tuples for missing fields
        http_status: HTTP status code (default 400)
    
    Returns:
        Tuple of (http_status, error_dict, content_type)
        
    Example field_errors:
        [("ObserverFullname", "ФИО инспектора"), ("Date", "Дата проверки")]
    
    This creates errordetails with target bindings that Fiori Elements
    can use to highlight specific fields in the UI.
    """
    errordetails = [
        {
            "code": "REQUIRED_FIELD",
            "message": {"value": f"Поле '{label}' обязательно для заполнения", "lang": "ru"},
            "severity": "error",
            "target": field_name,
        }
        for field_name, label in field_errors
    ]
    
    return format_error_response(
        code="VALIDATION_ERROR",
        message_value=message_value,
        severity="error",
        http_status=http_status,
        errordetails=errordetails,
    )


def format_child_validation_error(
    root_id: str,
    missing_children: list[dict[str, Any]],
    http_status: int = 400,
) -> tuple[int, dict[str, Any], str]:
    """Format validation error for missing required child row fields.
    
    Args:
        root_id: RootId of the parent entity
        missing_children: List of dicts with nav/id_prop/item_id/field/label
        http_status: HTTP status code (default 400)
    
    Returns:
        Tuple of (http_status, error_dict, content_type)
        
    Each missing_child dict should have:
        - nav: Navigation property name (e.g., "to_Checks")
        - id_prop: Key property name (e.g., "ItemId")
        - item_id: Actual key value
        - field: Field name (e.g., "Result")
        - label: Field label (e.g., "Результат")
    
    Creates navigation-qualified targets like "to_Checks/ItemId='xxx'/Result"
    that Fiori Elements can use to highlight specific child row fields.
    """
    errordetails = [
        {
            "code": "REQUIRED_CHILD_FIELD",
            "message": {
                "value": f"Поле '{item['label']}' обязательно для заполнения",
                "lang": "ru"
            },
            "severity": "error",
            "target": f"{item['nav']}/{item['id_prop']}='{item['item_id']}'/{item['field']}",
        }
        for item in missing_children
    ]
    
    return format_error_response(
        code="VALIDATION_ERROR",
        message_value=f"Заполните все обязательные поля в дочерних элементах (RootId={root_id})",
        severity="error",
        http_status=http_status,
        errordetails=errordetails,
    )


def format_draft_locked_error(owner_display_name: str) -> tuple[int, dict[str, Any], str]:
    """Format draft locked by another user error (409 Conflict).
    
    Args:
        owner_display_name: Display name of the user who owns the draft
    
    Returns:
        Tuple of (409, error_dict, content_type)
        
    This is the standard BOPF/Fiori draft protocol error when a draft
    is already being edited by another user.
    """
    return format_error_response(
        code="DRAFT_LOCKED",
        message_value=f"Черновик уже редактируется пользователем: {owner_display_name}",
        severity="error",
        http_status=409,
    )


def format_not_found_error(entity_set: str, key_value: str) -> tuple[int, dict[str, Any], str]:
    """Format resource not found error (404).
    
    Args:
        entity_set: Name of the entity set (e.g., "CheckRoots")
        key_value: Key value that was not found
    
    Returns:
        Tuple of (404, error_dict, content_type)
    """
    return format_error_response(
        code="NOT_FOUND",
        message_value=f"Ресурс '{entity_set}' с ключом '{key_value}' не найден",
        severity="error",
        http_status=404,
    )


def format_csrf_error() -> tuple[int, dict[str, Any], str]:
    """Format CSRF token validation error (403).
    
    Returns:
        Tuple of (403, error_dict, content_type)
    """
    return format_error_response(
        code="CSRF_VALIDATION_FAILED",
        message_value="CSRF token validation failed",
        severity="error",
        http_status=403,
    )


def format_internal_error(message_value: str) -> tuple[int, dict[str, Any], str]:
    """Format internal server error (500).
    
    Args:
        message_value: Error description (safe for client display)
    
    Returns:
        Tuple of (500, error_dict, content_type)
    """
    return format_error_response(
        code="INTERNAL_ERROR",
        message_value=message_value,
        severity="error",
        http_status=500,
    )
