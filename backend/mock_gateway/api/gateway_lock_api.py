"""LastChangeSet / LockStatusSet / lock_control (LockAcquire/Heartbeat/Release) routes."""
import json

from fastapi import APIRouter, Depends, Query, Request, Response
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistRoot
from services.current_user_service import CurrentUserService
from services.lock_service import LockService
from utils.odata import SERVICE_ROOT, format_datetime, format_entity_etag, odata_payload
from utils.odata_response import odata_entity
from api.gateway_operations import _hex
from api.gateway_core import (
    _err, _entity_key, _entity_metadata, _agg_changed_on, _build_lock_status_row,
    _apply_order_filter, _load_root_or_error, _require_permission,
    _resolve_lock_root_id, _normalize_root_filter_aliases, _merge_query_and_payload,
    _lock_import_payload,
)

router = APIRouter(tags=["GatewayCanonical"])


@router.get(f"{SERVICE_ROOT}/LastChangeSet({{entity_key}})")
def last_change_entity(entity_key: str, response: Response, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    agg = _agg_changed_on(root)
    root_key_hex = _hex(root.id)
    response.headers["ETag"] = format_entity_etag(agg, root.version_number)
    return odata_entity({
        "__metadata": _entity_metadata("LastChange", "LastChangeSet", root_key_hex),
        "DB_KEY": root_key_hex,
        "AggChangedOn": format_datetime(agg)
    })


@router.get(f"{SERVICE_ROOT}/LastChangeSet")
def last_change_set(
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    filter = _normalize_root_filter_aliases(filter)
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"DB_KEY": "id"},
        filter,
        orderby,
        top,
        skip,
    )
    payload = [{
        "__metadata": _entity_metadata("LastChange", "LastChangeSet", _hex(r.id)),
        "DB_KEY": _hex(r.id),
        "AggChangedOn": format_datetime(_agg_changed_on(r))
    } for r in rows]
    return odata_payload(payload, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/LockStatusSet")
def lock_status_set(
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    session_guid: str = Query("", alias="SessionGuid"),
    db: Session = Depends(get_db),
):
    filter = _normalize_root_filter_aliases(filter)
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"DB_KEY": "id"},
        filter,
        orderby,
        top,
        skip,
    )
    payload = [_build_lock_status_row(db, r.id, session_guid=session_guid) for r in rows]
    return odata_payload(payload, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/LockStatusSet({{entity_key}})")
def lock_status_entity(entity_key: str, session_guid: str = Query("", alias="SessionGuid"), db: Session = Depends(get_db)):
    return odata_entity(_build_lock_status_row(db, _entity_key(entity_key), session_guid=session_guid))


def lock_control(payload: dict, db: Session):
    action = str(payload.get("Action") or "").upper()
    root_key = str(payload.get("DB_KEY") or "")
    session = str(payload.get("SessionGuid") or "")
    uname = CurrentUserService.resolve_uname(db=db)
    if not root_key:
        return _err(400, "VALIDATION_ERROR", "DB_KEY is required")
    def _lock_entity(ok: bool, reason: str, expires_at, is_killed: bool, action_name: str, owner: str = "", owner_session: str = "", code: str = ""):
        lock_exp = format_datetime(expires_at)
        return odata_entity({
            "Success": ok,
            "Ok": ok,
            "Code": str(code or reason or "").strip(),
            "LockExpires": lock_exp,
            "ExpiresOn": lock_exp,
            "IsKilled": is_killed,
            "ReasonCode": reason,
            "Action": action_name,
            "Owner": owner,
            "OwnerSession": owner_session,
        })

    if root_key == "__CREATE":
        if action == "HEARTBEAT":
            return _lock_entity(True, "OWNED_BY_YOU", None, False, "HEARTBEAT", uname, session, "LOCK_OK")
        if action == "RELEASE":
            return _lock_entity(True, "FREE", None, False, "RELEASED", code="LOCK_OK")
        if action == "ACQUIRE":
            return _lock_entity(True, "OWNED_BY_YOU", None, False, "ACQUIRED", uname, session, "LOCK_OK")

    root_uuid = _entity_key(root_key)
    root_exists = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_uuid, ChecklistRoot.is_deleted.isnot(True)).first()
    if not root_exists:
        if action == "RELEASE":
            return _lock_entity(True, "FREE", None, False, "RELEASED", code="LOCK_OK")
        if action == "HEARTBEAT":
            return _lock_entity(False, "FREE", None, False, "FREE", code="LOCK_MISSING")
        return _err(404, "NOT_FOUND", "Checklist not found")

    if action == "ACQUIRE":
        r = LockService.acquire(
            db,
            root_uuid,
            session,
            uname,
            payload.get("StealFrom"),
            force_takeover=bool(payload.get("ForceTakeover"))
        )
        ok = bool(r.get("success"))
        return _lock_entity(
            ok,
            "OWNED_BY_YOU" if ok else "LOCKED_BY_OTHER",
            r.get("lock_expires"),
            bool(r.get("is_killed_flag")),
            str(r.get("action") or ("ACQUIRED" if ok else "FAILED")),
            str(r.get("owner") or (uname if ok else "")),
            str(r.get("owner_session") or (session if ok else "")),
            str(r.get("code") or ""),
        )
    if action == "HEARTBEAT":
        try:
            r = LockService.heartbeat(db, root_uuid, session)
            ok = bool(r.get("success"))
            return _lock_entity(
                ok,
                "OWNED_BY_YOU" if ok else "KILLED",
                r.get("lock_expires"),
                bool(r.get("is_killed")),
                str(r.get("action") or ("HEARTBEAT" if ok else "FAILED")),
                str(r.get("owner") or (uname if ok else "")),
                str(r.get("owner_session") or (session if ok else "")),
                str(r.get("code") or ""),
            )
        except ValueError as exc:
            code = str(exc)
            if code == "LOCK_MISSING":
                return _lock_entity(False, "FREE", None, False, "FREE", code="LOCK_MISSING")
            if code == "LOCK_NOT_OWNED_BY_SESSION":
                active_lock = LockService._active_lock(db, root_uuid)
                return _lock_entity(
                    False,
                    "LOCKED_BY_OTHER",
                    LockService._lock_expires_at(active_lock),
                    False,
                    "FAILED",
                    str(active_lock.user_id or "") if active_lock else "",
                    str(active_lock.session_guid or "") if active_lock else "",
                    "LOCK_NOT_OWNED_BY_SESSION",
                )
            return _err(410, "LOCK_EXPIRED", "Lock expired")
    if action == "RELEASE":
        r = LockService.release(db, root_uuid, session)
        released = bool(r.get("released"))
        return _lock_entity(
            released,
            str(r.get("reason_code") or ("FREE" if released else "LOCKED_BY_OTHER")),
            None,
            False,
            str(r.get("action") or ("RELEASED" if released else "FAILED")),
            str(r.get("owner") or ""),
            str(r.get("owner_session") or ""),
            code=str(r.get("code") or ""),
        )
    return _err(400, "VALIDATION_ERROR", "Unsupported Action")


@router.post(f"{SERVICE_ROOT}/LockAcquire")
async def lock_acquire_function_import(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _resolve_lock_root_id(merged, root_id)
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")
    # An edit lock is meaningless without edit rights - gate acquisition itself so a caller
    # can never obtain a lock (and, downstream, pass every LockService session-ownership
    # check) on a checklist they aren't authorized to edit. "__CREATE" is the not-yet-
    # persisted draft sentinel handled specially inside lock_control() - nothing to check yet.
    if resolved_root_id != "__CREATE":
        existing_root, load_err = _load_root_or_error(db, resolved_root_id)
        if existing_root and (err := _require_permission(db, request, existing_root, "edit")):
            return err
    return lock_control(_lock_import_payload("ACQUIRE", resolved_root_id, resolved_session_guid, merged), db)


@router.post(f"{SERVICE_ROOT}/LockHeartbeat")
async def lock_heartbeat_function_import(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _resolve_lock_root_id(merged, root_id)
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")
    return lock_control(_lock_import_payload("HEARTBEAT", resolved_root_id, resolved_session_guid, merged), db)


@router.post(f"{SERVICE_ROOT}/LockRelease")
async def lock_release_function_import(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _resolve_lock_root_id(merged, root_id)
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")
    return lock_control(_lock_import_payload("RELEASE", resolved_root_id, resolved_session_guid, merged), db)
