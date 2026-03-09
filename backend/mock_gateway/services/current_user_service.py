from __future__ import annotations

import json

from models import AppUserProfile, RuntimeUserContext

REQUEST_USER_HEADER = "X-PCCT-User"
DEFAULT_RUNTIME_UNAME = "operator"
PERMISSION_CODE_MAP = {
    "view": "01",
    "edit": "02",
    "create": "02",
    "delete": "03",
}


def _normalize_uname(value: str | None) -> str:
    return str(value or "").strip() or DEFAULT_RUNTIME_UNAME


def _normalize_permission_scope(item: dict) -> tuple[str, str]:
    scope_kind = str(item.get("scope") or item.get("scope_kind") or item.get("object_type") or "").strip().lower()
    scope_value = str(item.get("scopeValue") or item.get("scope_value") or item.get("bukrs") or item.get("object_value") or "").strip()
    if scope_kind == "bukrs" and scope_value:
        return "bukrs", scope_value
    return "all", "ALL"


def _normalize_permissions(value) -> list[dict]:
    if isinstance(value, str):
        try:
            value = json.loads(value)
        except Exception:
            value = [item.strip() for item in value.split(",") if item and item.strip()]
    if not isinstance(value, list):
        return []
    result = []
    seen = set()
    for item in value:
        if isinstance(item, dict):
            raw_code = str(item.get("code") or "").strip()
            raw_operation = str(item.get("operation") or item.get("permission") or "").strip().lower()
            code = raw_code or PERMISSION_CODE_MAP.get(raw_operation, "")
            if not code:
                continue
            scope_kind, scope_value = _normalize_permission_scope(item)
        else:
            raw_operation = str(item or "").strip().lower()
            code = PERMISSION_CODE_MAP.get(raw_operation, "")
            if not code:
                continue
            scope_kind, scope_value = "all", "ALL"
        resolved = f"{code}|{scope_kind}|{scope_value.upper()}"
        if resolved in seen:
            continue
        seen.add(resolved)
        result.append({
            "code": code,
            "scope_kind": scope_kind,
            "scope_value": scope_value,
        })
    return result


class CurrentUserService:
    @staticmethod
    def resolve_uname(db=None, request=None, explicit_uname: str | None = None) -> str:
        if explicit_uname:
            return _normalize_uname(explicit_uname)
        if request is not None:
            sHeaderUser = str(request.headers.get(REQUEST_USER_HEADER) or "").strip()
            if sHeaderUser:
                return _normalize_uname(sHeaderUser)
        if db is not None:
            row = db.get(RuntimeUserContext, "CURRENT")
            if row is not None:
                return _normalize_uname(getattr(row, "uname", None))
        return _normalize_uname(None)

    @staticmethod
    def resolve_profile(db, request=None, explicit_uname: str | None = None) -> dict:
        uname = CurrentUserService.resolve_uname(db=db, request=request, explicit_uname=explicit_uname)
        row = db.get(AppUserProfile, uname) if db is not None else None
        permissions = _normalize_permissions(getattr(row, "permissions_json", []))
        full_name = str(getattr(row, "full_name", "") or "").strip() or uname
        return {
            "uname": uname,
            "full_name": full_name,
            "permissions": permissions
        }
