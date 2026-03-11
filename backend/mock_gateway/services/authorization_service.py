from __future__ import annotations

from services.current_user_service import CurrentUserService
from models import ChecklistRoot

AUTH_OBJECT_CHECKLIST = "Z_UI5_CHKL"
OPERATION_CREATE = "01"
OPERATION_CHANGE = "02"
OPERATION_VIEW = "03"
OPERATION_DELETE = "06"

_VIEW_DENY_MARKERS = ("noview", "denyview", "noaccess", "denyall")
_EDIT_DENY_MARKERS = ("noedit", "denyedit", "readonly", "viewonly", "denyall")
_DELETE_DENY_MARKERS = ("nodelete", "denydelete", "readonly", "viewonly", "denyall")


def _normalize_user_id(user_id: str | None, db=None) -> str:
    resolved = str(user_id or "").strip()
    return resolved or CurrentUserService.resolve_uname(db=db)


def _contains_marker(user_id: str, markers: tuple[str, ...]) -> bool:
    lowered = user_id.lower()
    return any(marker in lowered for marker in markers)


def _granted_operations(can_view: bool, can_edit: bool, can_delete: bool) -> list[str]:
    ops = []
    if can_edit:
        ops.append(OPERATION_CHANGE)
    if can_view:
        ops.append(OPERATION_VIEW)
    if can_delete:
        ops.append(OPERATION_DELETE)
    return ops


def _has_create_permission(db, user_id: str) -> bool:
    profile = CurrentUserService.resolve_profile(db, explicit_uname=user_id)
    permissions = list(profile.get("permissions") or [])
    return any(str((item or {}).get("code") or "").strip() == OPERATION_CREATE for item in permissions)


def _permission_matches_scope(root: ChecklistRoot, permission: dict) -> bool:
    scope_kind = str((permission or {}).get("scope_kind") or "all").strip().lower()
    scope_value = str((permission or {}).get("scope_value") or "ALL").strip().upper() or "ALL"
    if scope_kind == "all" or scope_value == "ALL":
        return True
    if scope_kind == "bukrs":
        root_bukrs = str(getattr(root, "bukrs", "") or "").strip().upper()
        return bool(root_bukrs) and root_bukrs == scope_value
    return False


def _permissions_from_profile(db, root: ChecklistRoot, user_id: str) -> tuple[bool, bool, bool] | None:
    profile = CurrentUserService.resolve_profile(db, explicit_uname=user_id)
    permissions = list(profile.get("permissions") or [])
    if not permissions:
        return None
    permission_codes = {
        str((item or {}).get("code") or "").strip()
        for item in permissions
        if _permission_matches_scope(root, item or {})
    }
    return (
        "03" in permission_codes,
        "02" in permission_codes,
        "06" in permission_codes
    )


def _permission_payload(root: ChecklistRoot, user_id: str, can_view: bool, can_edit: bool, can_delete: bool, reason_code: str, message: str) -> dict:
    return {
        "root_id": str(root.id or ""),
        "user_id": user_id,
        "auth_object": AUTH_OBJECT_CHECKLIST,
        "create_operation": OPERATION_CREATE,
        "view_operation": OPERATION_VIEW,
        "change_operation": OPERATION_CHANGE,
        "delete_operation": OPERATION_DELETE,
        "granted_operations": _granted_operations(can_view, can_edit, can_delete),
        "can_create": False,
        "can_view": bool(can_view),
        "can_edit": bool(can_edit),
        "can_delete": bool(can_delete),
        "reason_code": reason_code,
        "message": message,
    }


class AuthorizationService:
    @staticmethod
    def create_permission(user_id: str | None, db=None) -> dict:
        resolved_user = _normalize_user_id(user_id, db=db)
        can_create = _has_create_permission(db, resolved_user) if db is not None else False
        return {
            "root_id": "",
            "user_id": resolved_user,
            "auth_object": AUTH_OBJECT_CHECKLIST,
            "create_operation": OPERATION_CREATE,
            "view_operation": OPERATION_VIEW,
            "change_operation": OPERATION_CHANGE,
            "delete_operation": OPERATION_DELETE,
            "granted_operations": [OPERATION_CREATE] if can_create else [],
            "can_create": can_create,
            "can_view": False,
            "can_edit": False,
            "can_delete": False,
            "reason_code": "AUTHORIZED" if can_create else "NO_CREATE_AUTH",
            "message": "Checklist create access granted" if can_create else "No permission to create a checklist",
        }

    @staticmethod
    def checklist_permissions(root: ChecklistRoot, user_id: str | None, db=None) -> dict:
        resolved_user = _normalize_user_id(user_id, db=db)
        resolved_from_profile = _permissions_from_profile(db, root, resolved_user) if db is not None else None

        if resolved_from_profile is not None:
            can_view, can_edit, can_delete = resolved_from_profile
            if not can_view:
                return _permission_payload(
                    root,
                    resolved_user,
                    False,
                    False,
                    False,
                    "NO_VIEW_AUTH",
                    "No permission to open this checklist",
                )
            if not can_edit and not can_delete:
                return _permission_payload(
                    root,
                    resolved_user,
                    True,
                    False,
                    False,
                    "READ_ONLY_AUTH",
                    "Read-only access for this checklist",
                )
            if not can_edit:
                return _permission_payload(
                    root,
                    resolved_user,
                    True,
                    False,
                    can_delete,
                    "NO_EDIT_AUTH",
                    "No permission to edit this checklist",
                )
            if not can_delete:
                return _permission_payload(
                    root,
                    resolved_user,
                    True,
                    True,
                    False,
                    "NO_DELETE_AUTH",
                    "No permission to delete this checklist",
                )
            return _permission_payload(
                root,
                resolved_user,
                True,
                True,
                True,
                "AUTHORIZED",
                "Checklist access granted",
            )

        if _contains_marker(resolved_user, _VIEW_DENY_MARKERS):
            return _permission_payload(
                root,
                resolved_user,
                False,
                False,
                False,
                "NO_VIEW_AUTH",
                "No permission to open this checklist",
            )

        can_edit = not _contains_marker(resolved_user, _EDIT_DENY_MARKERS)
        can_delete = not _contains_marker(resolved_user, _DELETE_DENY_MARKERS)

        if not can_edit and not can_delete:
            return _permission_payload(
                root,
                resolved_user,
                True,
                False,
                False,
                "READ_ONLY_AUTH",
                "Read-only access for this checklist",
            )
        if not can_edit:
            return _permission_payload(
                root,
                resolved_user,
                True,
                False,
                can_delete,
                "NO_EDIT_AUTH",
                "No permission to edit this checklist",
            )
        if not can_delete:
            return _permission_payload(
                root,
                resolved_user,
                True,
                True,
                False,
                "NO_DELETE_AUTH",
                "No permission to delete this checklist",
            )

        return _permission_payload(
            root,
            resolved_user,
            True,
            True,
            True,
            "AUTHORIZED",
            "Checklist access granted",
        )
