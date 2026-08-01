"""Auth/lock/etag kernel for the canonical Gateway API: key normalization, permission
gates, optimistic-concurrency checks, and lock-status/lock-import-payload helpers.

Field maps and OData entity serializers live in api.gateway_serializers; the Check/
Barrier _DetailKind machinery lives in api.gateway_detail_kind; SaveChanges/AutoSave
row-mutation helpers live in api.gateway_mutations. The route modules
(api/gateway_*_api.py) import from all four and only define @router routes.
"""
import re
from datetime import datetime, timezone

from fastapi import Request
from sqlalchemy import asc, desc
from sqlalchemy.orm import Session

from config import MAX_PAGE_SIZE
from models import ChecklistRoot, ChecklistRootDraft, LockEntry
from services.authorization_service import AuthorizationService
from services.current_user_service import CurrentUserService
from utils.filter_parser import FilterParser
from utils.odata import SERVICE_ROOT, format_datetime, odata_error_response
from utils.odata_etag import etag_for_datetime, validate_if_match
from utils.time import now_utc
from api.gateway_operations import _hex
from api.gateway_validators import _status_external
from api.gateway_helpers import BoundaryResolver, HEX_ZERO_GUID

_STATUS_TRANSITIONS = {
    "DRAFT": {"REGISTERED"},
    "REGISTERED": {"CLOSED", "DRAFT"},
    "CLOSED": set(),
}


def _err(status: int, code: str, message: str):
    return odata_error_response(status, code, message)


def _reject_expand(expand: str | None):
    if expand:
        return _err(400, "VALIDATION_ERROR", "EXPAND_NOT_ALLOWED")
    return None


def _entity_key(key_expr: str) -> str:
    """Wrapper for BoundaryResolver.resolve_key for backwards compatibility."""
    return BoundaryResolver.resolve_key(key_expr)


def _boundary_root_key(payload: dict | None = None, *candidates) -> str:
    """Wrapper for BoundaryResolver.resolve_root_key for backwards compatibility."""
    return BoundaryResolver.resolve_root_key(payload, *candidates)


def _resolve_lock_root_id(payload: dict | None, root_id: str | None) -> str:
    """Resolve DB_KEY for the Lock* function imports.

    Unlike _boundary_root_key, this must NOT filter out the "__CREATE" sentinel:
    lock_control() explicitly special-cases root_key == "__CREATE" (a not-yet-persisted
    draft still needs to answer lock heartbeat/release calls as a benign no-op).
    """
    payload = payload if isinstance(payload, dict) else {}
    for candidate in (root_id, payload.get("DB_KEY"), payload.get("db_key")):
        if candidate:
            raw = str(candidate).strip()
            if raw:
                return raw
    return ""


def _boundary_parent_key(payload: dict | None = None, *candidates) -> str:
    """Wrapper for BoundaryResolver.resolve_parent_key for backwards compatibility."""
    return BoundaryResolver.resolve_parent_key(payload, *candidates)


def _entity_metadata(entity_type: str, entity_set: str, key_value: str) -> dict:
    safe_key = str(key_value or "").replace("'", "''")
    return {
        "type": f"Z_EHS_PRODUCTION_CONTROL_CKLT_SRV.{entity_type}",
        "uri": f"{SERVICE_ROOT}/{entity_set}('{safe_key}')",
    }


def _agg_changed_on(root: ChecklistRoot):
    points = [root.changed_on, root.created_on]
    points.extend([c.changed_on for c in (root.checks or []) if c.changed_on])
    points.extend([b.changed_on for b in (root.barriers or []) if b.changed_on])
    points = [p for p in points if p is not None]
    return max(points) if points else now_utc()


def _is_expired_lock(expires_on: datetime | None) -> bool:
    if not expires_on:
        return False
    dt = expires_on if expires_on.tzinfo else expires_on.replace(tzinfo=timezone.utc)
    return dt < now_utc()


def _build_lock_status_row(db: Session, root_uuid: str, session_guid: str = "") -> dict:
    root_key_hex = _hex(root_uuid)
    meta = _entity_metadata("LockStatus", "LockStatusSet", root_key_hex)

    def _row(ok: bool, reason: str, owner: str, expires_at) -> dict:
        exp = format_datetime(expires_at)
        return {
            "__metadata": meta,
            "DB_KEY": root_key_hex,
            "Ok": ok,
            "Success": ok,
            "ReasonCode": reason,
            "Owner": owner,
            "ExpiresOn": exp,
            "LockExpires": exp,
        }

    if root_uuid == "__CREATE":
        return _row(bool(session_guid), "OWNED_BY_YOU" if session_guid else "FREE", CurrentUserService.resolve_uname(db=db) if session_guid else "", None)

    active = db.query(LockEntry).filter(LockEntry.db_key == root_uuid, LockEntry.is_killed.is_(False)).first()
    if not active:
        own = db.query(LockEntry).filter(
            LockEntry.db_key == root_uuid,
            LockEntry.session_guid == session_guid,
        ).order_by(LockEntry.last_refresh_at.desc()).first() if session_guid else None
        if own:
            if own.is_killed:
                return _row(False, "KILLED", own.user_id or "", own.expires_at)
            if _is_expired_lock(own.expires_at):
                own.is_killed = True
                db.commit()
                return _row(False, "EXPIRED", own.user_id or "", own.expires_at)
        return _row(True, "FREE", "", None)
    if _is_expired_lock(active.expires_at):
        active.is_killed = True
        db.commit()
        return _row(False, "EXPIRED", active.user_id or "", active.expires_at)
    if active.session_guid == session_guid and session_guid:
        return _row(True, "OWNED_BY_YOU", active.user_id, active.expires_at)
    return _row(False, "LOCKED_BY_OTHER", active.user_id, active.expires_at)


def _parse_odata_datetime(value: str | None) -> datetime:
    if not value:
        return now_utc()
    raw = str(value).strip()
    if raw.startswith("/Date(") and raw.endswith(")/"):
        body = raw[6:-2]
        sign_pos = max(body.find("+", 1), body.find("-", 1))
        millis_part = body if sign_pos < 0 else body[:sign_pos]
        try:
            return datetime.fromtimestamp(int(millis_part) / 1000, tz=timezone.utc)
        except ValueError:
            return now_utc()
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    return datetime.fromisoformat(raw.replace("Z", "+00:00"))


def _apply_select(row: dict, select_expr: str | None) -> dict:
    if not select_expr:
        return row
    keep = {x.strip() for x in str(select_expr).split(",") if x.strip()}
    selected = {k: v for k, v in row.items() if k in keep}
    if "__metadata" in row:
        selected["__metadata"] = row["__metadata"]
    return selected


def _apply_order_filter(query, model, fmap, filter_expr, orderby, top, skip):
    expr = FilterParser.parse(model, filter_expr, field_map=fmap)
    if expr is not None:
        query = query.filter(expr)
    if orderby:
        for clause in [x.strip() for x in orderby.split(",") if x.strip()]:
            field, _, direction = clause.partition(" ")
            col = getattr(model, fmap.get(field, field), None)
            if col is not None:
                query = query.order_by(desc(col) if direction.lower() == "desc" else asc(col))
    total = query.count()
    # Shared query-execution point for every ChecklistRootSet/ChecklistCheckSet/
    # ChecklistBarrierSet/etc. list route - clamp here once rather than trusting every
    # call site to have already gone through parse_paging()'s MAX_PAGE_SIZE cap.
    bounded_top = min(max(0, int(top or 0)), MAX_PAGE_SIZE)
    bounded_skip = max(0, int(skip or 0))
    return query.offset(bounded_skip).limit(bounded_top).all(), total


def _if_match_check(if_match: str | None, agg: datetime, version_number: int | None = None):
    if not validate_if_match(if_match, etag_for_datetime(agg, version_number)):
        raise ValueError("PRECONDITION_FAILED")


def _optimistic_check(client_agg: str | None, root: ChecklistRoot):
    if not client_agg:
        raise ValueError("VALIDATION_ERROR")
    cms = int(_parse_odata_datetime(client_agg).timestamp() * 1000)
    current_dt = _agg_changed_on(root)
    current_dt = current_dt if current_dt.tzinfo else current_dt.replace(tzinfo=timezone.utc)
    current = int(current_dt.timestamp() * 1000)
    if cms != current:
        raise ValueError("CONFLICT")


def _load_root_or_error(db: Session, root_key: str):
    try:
        _entity_key(root_key)
    except ValueError:
        return None, _err(400, "VALIDATION_ERROR", "DB_KEY must be RAW16 HEX (32 chars)")
    root = db.query(ChecklistRoot).filter(
        ChecklistRoot.id == _entity_key(root_key),
        ChecklistRoot.is_deleted.isnot(True),
    ).first()
    if not root:
        return None, _err(404, "NOT_FOUND", "Checklist not found")
    return root, None


def _load_root_or_draft_or_error(db: Session, entity_key: str):
    """Draft-aware sibling of _load_root_or_error - additive, does not replace it.

    Returns (kind, obj, err) where kind is "active"|"draft"|None. Only takes the draft
    branch when entity_key matches the compound ActiveUUID/DraftUUID form; every existing
    flat-hex/business-key call site keeps going through the unmodified _load_root_or_error.
    """
    parsed = BoundaryResolver.resolve_draft_key(entity_key)
    if parsed is None:
        root, err = _load_root_or_error(db, entity_key)
        return ("active", root, None) if root else (None, None, err)

    active_hex, draft_hex = parsed
    if draft_hex in ("", HEX_ZERO_GUID):
        # Compound key addressing the ACTIVE side (DraftUUID=zero) - resolve exactly like
        # the flat key would.
        root, err = _load_root_or_error(db, active_hex)
        return ("active", root, None) if root else (None, None, err)

    draft = db.query(ChecklistRootDraft).filter(
        ChecklistRootDraft.id == _entity_key(draft_hex)
    ).first()
    if not draft:
        return None, None, _err(404, "NOT_FOUND", "Draft not found")
    return "draft", draft, None


_PERMISSION_FLAGS = {"view": "can_view", "edit": "can_edit", "delete": "can_delete"}


def _require_permission(db: Session, request: Request, root: ChecklistRoot, need: str):
    """Enforce AuthorizationService.checklist_permissions() before a mutation, instead of
    only reporting it to the client via the read-only ChecklistPermissionSet endpoint.
    Without this, any caller holding (or able to acquire) an edit lock could write to a
    checklist regardless of CanView/CanEdit/CanDelete - the permission model existed only
    as UI guidance, never as a server-side gate. Returns an error Response if denied, else
    None so callers can `if (err := _require_permission(...)): return err`.
    """
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    permission = AuthorizationService.checklist_permissions(root, resolved_uname, db=db)
    if not permission.get(_PERMISSION_FLAGS[need]):
        return _err(403, "FORBIDDEN", str(permission.get("message") or "Access denied"))
    return None


def _require_create_permission(db: Session, request: Request):
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    permission = AuthorizationService.create_permission(resolved_uname, db=db)
    if not permission.get("can_create"):
        return _err(403, "FORBIDDEN", str(permission.get("message") or "Access denied"))
    return None


def _validate_status_change(root: ChecklistRoot, new_status: str):
    old_status = _status_external(root.status)
    if old_status == new_status:
        return
    allowed = _STATUS_TRANSITIONS.get(old_status)
    if allowed is not None and new_status not in allowed:
        raise ValueError("VALIDATION_ERROR")


def _import_payload(root_id: str | None = None, session_guid: str | None = None, status: str | None = None, payload: dict | None = None) -> dict:
    body = payload or {}
    # root_id is already fully resolved by the caller (may legitimately be the "__CREATE"
    # sentinel for lock imports) - do not re-run it through _boundary_root_key, which would
    # filter "__CREATE" back out to "".
    result = {
        "DB_KEY": str(root_id or "").strip() or _boundary_root_key(body),
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Status": body.get("Status") or body.get("status") or status,
        "Changes": body.get("Changes") or [],
        "ClientAggChangedOn": body.get("ClientAggChangedOn"),
        "FullPayload": body.get("FullPayload") or body,
    }
    # Preserve lock-specific fields that must not be dropped
    for _k in ("Force", "StealFrom", "NewStatus"):
        if _k in body:
            result[_k] = body[_k]
    return result


def _normalize_root_filter_aliases(filter_value: str | None) -> str:
    return re.sub(r"\bRootKey\b|\bRootId\b", "PARENT_KEY", str(filter_value or ""), flags=re.IGNORECASE)


def _merge_query_and_payload(request: Request, payload: dict | None = None) -> dict:
    merged = dict(request.query_params)
    merged.update(payload or {})
    return merged


def _lock_import_payload(action: str, root_id: str, session_guid: str, payload: dict | None) -> dict:
    req = _import_payload(root_id=root_id, session_guid=session_guid, payload=payload)
    req["Action"] = action
    return req
