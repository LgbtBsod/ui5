"""AutoSave / CreateChecklist / CopyChecklist / SaveChanges / SetChecklistStatus /
GetHierarchy / ReportExport routes."""
import json
import uuid
from datetime import datetime, timezone

from fastapi import APIRouter, Depends, Header, Query, Request, Response
from sqlalchemy.orm import Session

from config import LOCK_TTL
from database import get_db
from models import ChecklistRoot, DictionaryItem
from services.analytics_service import AnalyticsService
from services.checklist_service import ChecklistService
from services.current_user_service import CurrentUserService
from services.hierarchy_service import HierarchyService
from services.lock_service import LockService
from utils.odata import SERVICE_ROOT, format_datetime
from utils.odata_response import odata_collection, odata_entity
from utils.time import now_utc
from utils.sap_message import build_sap_message
from api.gateway_operations import _apply_save_attachments, _replace_detail_rows, _hex, _normalize_hex_key
from api.gateway_validators import _pick_text, _status_external, _normalize_basic_payload, _normalize_status_input
from api.gateway_root_api import _build_new_root
from api.gateway_core import (
    _err, _boundary_root_key, _agg_changed_on, _parse_odata_datetime, _if_match_check,
    _optimistic_check, _load_root_or_error, _require_permission, _require_create_permission,
    _validate_status_change, _merge_query_and_payload, _entity_key,
)
from api.gateway_detail_kind import _CHECK_KIND, _BARRIER_KIND
from api.gateway_serializers import (
    _normalize_export_limit, _normalize_export_search_contract, _search_contract_matches,
    _to_search, _to_root, _to_check, _to_barrier,
)
from api.gateway_mutations import (
    _save_request_root, _save_request_root_key, _save_request_session,
    _apply_save_root, _apply_save_detail_rows,
)

router = APIRouter(tags=["GatewayCanonical"])


@router.post(f"{SERVICE_ROOT}/AutoSave")
def auto_save(payload: dict, response: Response, request: Request, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    root, err = _load_root_or_error(db, _save_request_root_key(payload))
    if err:
        return err
    if (err := _require_permission(db, request, root, "edit")):
        return err
    session_guid = _save_request_session(payload)
    if not session_guid:
        return _err(400, "VALIDATION_ERROR", "SessionGuid is required")
    try:
        LockService.validate_session_lock(db, root.id, session_guid)
    except ValueError as ex:
        if str(ex) in {"LOCK_MISSING", "LOCK_NOT_OWNED_BY_SESSION"}:
            return _err(409, str(ex), "Active lock for session is required")
        return _err(409, "LOCK_EXPIRED", "Lock expired")

    _apply_save_root(root, _save_request_root(payload), db)
    _apply_save_detail_rows(db, root, body.get("checks"), _CHECK_KIND)
    _apply_save_detail_rows(db, root, body.get("barriers"), _BARRIER_KIND)
    active_lock = LockService._active_lock(db, root.id)
    lock_refreshed_at = now_utc()
    if active_lock and str(active_lock.session_guid or "").strip() == str(session_guid or "").strip():
        active_lock.last_refresh_at = lock_refreshed_at
        active_lock.expires_at = lock_refreshed_at + LOCK_TTL
    root.changed_by = CurrentUserService.resolve_uname(db=db) or root.changed_by or "ANON"
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    active_lock = LockService._active_lock(db, root.id)
    request_id = str(uuid.uuid4())
    response.headers["sap-message"] = build_sap_message("Autosave completed", "success", code="SAVED")
    return odata_entity({
        "db_key": _hex(root.id),
        "changed_on": format_datetime(root.changed_on),
        "version_number": int(root.version_number or 0),
        "code": "LOCK_OK",
        "reason_code": "SAVED",
        "lock_refreshed": True,
        "lock_expires_at": format_datetime(LockService._lock_expires_at(active_lock)) if active_lock else "",
        "server_now": format_datetime(now_utc()),
        "request_id": request_id
    })


@router.post(f"{SERVICE_ROOT}/CreateChecklist")
def create_checklist(payload: dict, response: Response, request: Request, db: Session = Depends(get_db)):
    if (err := _require_create_permission(db, request)):
        return err
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    full = body.get("FullPayload") or body
    root_block = full.get("root") if isinstance(full, dict) else {}
    basic_values = _normalize_basic_payload(full.get("basic"), db)
    requested_id = _pick_text(root_block, "Id", "RequestId")
    requested_status = _pick_text(root_block, "Status", "status") or "DRAFT"
    user_name = CurrentUserService.resolve_uname(db=db)

    root = _build_new_root(
        db,
        checklist_id=requested_id,
        lpc=str(basic_values.get("lpc") or ""),
        status=requested_status,
        user_name=user_name,
        basic_values=basic_values,
    )
    db.add(root)
    db.flush()
    _replace_detail_rows(db, root, full.get("checks"), full.get("barriers"))
    try:
        _apply_save_attachments(db, root, full.get("attachments"))
    except ValueError:
        db.rollback()
        return _err(400, "INVALID_ATTACHMENT_PAYLOAD", "Attachment payload is invalid")
    except RuntimeError as ex:
        db.rollback()
        return ex.args[0]
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()

    out = _to_root(root, db=db)
    out["DB_KEY"] = out["DB_KEY"]
    out["AggChangedOn"] = format_datetime(_agg_changed_on(root))
    out["Message"] = "Checklist created"
    out["ReasonCode"] = "CREATED"
    response.headers["sap-message"] = build_sap_message("Checklist created", "success", code="CREATED")
    return odata_entity(out)


@router.post(f"{SERVICE_ROOT}/CopyChecklist")
async def copy_checklist(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _boundary_root_key(merged, root_id, merged.get("source_parent_key"), merged.get("SourceParentKey"), merged.get("SourceUuid"))
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")

    source_root, err = _load_root_or_error(db, resolved_root_id)
    if err:
        return err
    if (err := _require_permission(db, request, source_root, "view")):
        return err
    if (err := _require_create_permission(db, request)):
        return err

    try:
        copied_root = ChecklistService.copy(db, source_root.id, str(resolved_uname))
        lock_result = LockService.acquire(db, copied_root.id, str(resolved_session_guid).strip(), str(resolved_uname), None)
    except ValueError:
        return _err(404, "NOT_FOUND", "Checklist not found")

    if not lock_result.get("success"):
        return _err(409, "LOCK_REQUIRED", "Unable to lock copied checklist")

    AnalyticsService.mark_dirty()
    copied_root = db.query(ChecklistRoot).filter(ChecklistRoot.id == copied_root.id).first()
    response_payload = {
        "RequestId": copied_root.checklist_id or "",
        "Ok": True,
        "Success": True,
        "Message": "Checklist copied",
        "AggChangedOn": format_datetime(_agg_changed_on(copied_root)),
        "ReasonCode": "COPIED",
        "ExpiresOn": format_datetime(lock_result.get("lock_expires")),
        "LockExpires": format_datetime(lock_result.get("lock_expires")),
        "IsKilled": False,
        "Status": _status_external(copied_root.status),
        "DB_KEY": _hex(copied_root.id),
    }
    return odata_entity(response_payload)


@router.post(f"{SERVICE_ROOT}/SaveChanges")
def save_changes(payload: dict, response: Response, request: Request, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    root, err = _load_root_or_error(db, _save_request_root_key(payload))
    if err:
        return err
    if (err := _require_permission(db, request, root, "edit")):
        return err
    session_guid = _save_request_session(payload)
    if not session_guid:
        return _err(400, "VALIDATION_ERROR", "SessionGuid is required")
    try:
        LockService.validate_session_lock(db, root.id, session_guid)
    except ValueError as ex:
        if str(ex) in {"LOCK_MISSING", "LOCK_NOT_OWNED_BY_SESSION"}:
            return _err(409, str(ex), "Active lock for session is required")
        return _err(409, "LOCK_EXPIRED", "Lock expired")

    _apply_save_root(root, _save_request_root(payload), db)
    _apply_save_detail_rows(db, root, body.get("checks"), _CHECK_KIND)
    _apply_save_detail_rows(db, root, body.get("barriers"), _BARRIER_KIND)
    try:
        _apply_save_attachments(db, root, body.get("attachments"))
    except ValueError as ex:
        db.rollback()
        if str(ex) == "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN":
            return _err(400, "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN", "Attachment upload must use media stream endpoint")
        return _err(400, "INVALID_ATTACHMENT_PAYLOAD", "Attachment payload is invalid")
    except RuntimeError as ex:
        db.rollback()
        return ex.args[0]
    active_lock = LockService._active_lock(db, root.id)
    lock_refreshed_at = now_utc()
    if active_lock and str(active_lock.session_guid or "").strip() == str(session_guid or "").strip():
        active_lock.last_refresh_at = lock_refreshed_at
        active_lock.expires_at = lock_refreshed_at + LOCK_TTL
    root.changed_by = CurrentUserService.resolve_uname(db=db) or root.changed_by or "ANON"
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    active_lock = LockService._active_lock(db, root.id)
    request_id = str(uuid.uuid4())
    response.headers["sap-message"] = build_sap_message("Checklist updated", "success", code="SAVED")
    return odata_entity({
        "db_key": _hex(root.id),
        "changed_on": format_datetime(root.changed_on),
        "version_number": int(root.version_number or 0),
        "code": "LOCK_OK",
        "reason_code": "SAVED",
        "lock_refreshed": True,
        "lock_expires_at": format_datetime(LockService._lock_expires_at(active_lock)) if active_lock else "",
        "server_now": format_datetime(now_utc()),
        "request_id": request_id
    })


@router.post(f"{SERVICE_ROOT}/SetChecklistStatus")
async def set_status(
    request: Request,
    response: Response,
    root_key_q: str | None = Query(None, alias="DB_KEY"),
    new_status_q: str | None = Query(None, alias="NewStatus"),
    client_agg_q: str | None = Query(None, alias="ClientAggChangedOn"),
    if_match: str | None = Header(None, alias="If-Match"),
    db: Session = Depends(get_db),
):
    # SAP UI5 callFunction (POST) sends params as URL query string.
    # Fall back to JSON body for direct POST callers.
    body: dict = {}
    try:
        body = await request.json()
    except (json.JSONDecodeError, ValueError):
        body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_key = _boundary_root_key(merged, root_key_q)
    resolved_new_status = new_status_q or merged.get("NewStatus") or merged.get("new_status") or merged.get("Status") or merged.get("status") or ""
    resolved_client_agg = client_agg_q or merged.get("ClientAggChangedOn") or merged.get("client_agg_changed_on")

    root, err = _load_root_or_error(db, resolved_root_key)
    if err:
        return err
    if (err := _require_permission(db, request, root, "edit")):
        return err
    try:
        _if_match_check(if_match, _agg_changed_on(root), root.version_number)
        _optimistic_check(resolved_client_agg, root)
    except ValueError as ex:
        if str(ex) == "PRECONDITION_FAILED":
            return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
        if str(ex) == "VALIDATION_ERROR":
            return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
        return _err(409, "CONFLICT", "AggChangedOn mismatch")

    # _normalize_status_input() accepts both external names and internal codes but always
    # *returns* an internal code ("01"/"02"/"03") - and silently falls back to "01" (DRAFT)
    # for anything unrecognized. Validate the raw input against the accepted vocabulary
    # BEFORE normalizing, otherwise garbage input (or a stale/unsupported status name)
    # would silently succeed as DRAFT instead of being rejected; and run the transition
    # check (_validate_status_change / _STATUS_TRANSITIONS) against the external name,
    # since that's what it - and root.status's external representation - are keyed on.
    if str(resolved_new_status or "").strip().upper() not in {"DRAFT", "01", "REGISTERED", "02", "CLOSED", "03"}:
        return _err(400, "VALIDATION_ERROR", "Unsupported status")
    internal_status = _normalize_status_input(resolved_new_status)
    external_status = _status_external(internal_status)
    try:
        _validate_status_change(root, external_status)
    except ValueError:
        return _err(400, "VALIDATION_ERROR", "Invalid status transition")
    root.status = internal_status
    root.changed_on = now_utc()
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    response.headers["sap-message"] = build_sap_message("Status updated", "success", code="STATUS_SET")
    return odata_entity({"DB_KEY": _hex(root.id), "Status": _status_external(root.status), "AggChangedOn": format_datetime(_agg_changed_on(root)), "Message": "Status updated", "ReasonCode": "STATUS_SET"})


@router.get(f"{SERVICE_ROOT}/GetHierarchy")
@router.post(f"{SERVICE_ROOT}/GetHierarchy")
async def get_hierarchy(request: Request, date_check: str | None = Query(None, alias="DateCheck"), method: str | None = Query(None, alias="Method"), db: Session = Depends(get_db)):
    payload = {}
    if request.method == "POST":
        try:
            payload = await request.json()
        except (json.JSONDecodeError, ValueError):
            payload = {}
    date_value = payload.get("DateCheck") or payload.get("date") or date_check
    method_value = str(payload.get("Method") or payload.get("method") or method or "location_tree").lower()
    # Only location_tree is implemented; other values are a no-op but not an error
    if method_value not in ("location_tree", ""):
        pass  # silently ignore unknown methods, return tree anyway
    try:
        dt = _parse_odata_datetime(date_value)
    except (ValueError, TypeError, AttributeError):
        dt = now_utc()
    nodes = HierarchyService.get_tree(db, dt.date())
    return odata_entity({"DateCheck": format_datetime(datetime(dt.year, dt.month, dt.day, tzinfo=timezone.utc)), "results": nodes})


@router.post(f"{SERVICE_ROOT}/ReportExport")
def report_export(payload: dict, db: Session = Depends(get_db)):
    payload = payload if isinstance(payload, dict) else {}
    selection_mode = str(payload.get("SelectionMode") or "").strip().lower()
    entity = str(payload.get("Entity") or "screen").strip().lower() or "screen"
    export_limit = _normalize_export_limit(payload.get("Limit") or payload.get("limit"))
    root_keys = (
        payload.get("RootKeys")
        or payload.get("DBKeys")
        or payload.get("keys")
        or payload.get("ids")
        or []
    )

    if not root_keys:
        single_key = _boundary_root_key(payload)
        if single_key:
            root_keys = [single_key]

    root_keys = [_normalize_hex_key(value) for value in root_keys if _normalize_hex_key(value)]
    if root_keys and len(root_keys) > export_limit:
        return _err(400, "EXPORT_LIMIT_EXCEEDED", "Selected export exceeds configured limit")

    roots = []
    if root_keys:
        roots_by_key = {}
        for key in root_keys:
            root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(str(key)), ChecklistRoot.is_deleted.isnot(True)).first()
            if root:
                roots_by_key[str(key)] = root
        roots = [roots_by_key[key] for key in root_keys if key in roots_by_key]
    elif selection_mode == "all":
        search_contract = _normalize_export_search_contract(payload.get("SearchContract"))
        roots = [
            root for root in db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)).all()
            if _search_contract_matches(root, search_contract, db)
        ]

    # Preload CHECK/BARRIER dictionary texts once instead of one query per row - the loop
    # below can run over thousands of check/barrier rows for a single "all" export.
    check_dict_texts = {item.key: item.text for item in db.query(DictionaryItem).filter(DictionaryItem.domain == "CHECK").all()}
    barrier_dict_texts = {item.key: item.text for item in db.query(DictionaryItem).filter(DictionaryItem.domain == "BARRIER").all()}

    rows = []
    for root in roots:
        base = _to_search(root, db=db)
        checks = [_to_check(c) for c in root.checks]
        barriers = [_to_barrier(b) for b in root.barriers]
        base_row = {
            "DB_KEY": base["DB_KEY"],
            "Id": base["Id"],
            "Lpc": base.get("Lpc", ""),
            "LpcText": base.get("LpcText", ""),
            "Profession": base.get("Profession", ""),
            "ProfessionText": base.get("ProfessionText", ""),
            "LocationKey": base.get("LocationKey", ""),
            "Status": base.get("Status", ""),
            "EquipName": base.get("EquipName", ""),
            "ChangedOn": base.get("ChangedOn", ""),
            "DateCheck": base["DateCheck"],
        }

        if entity == "screen":
            rows.append({
                **base_row,
                "ItemType": "ROOT",
                "Num": 0,
                "Text": "", "Comment": "", "Result": None
            })
            continue

        if entity == "check":
            for c in checks:
                key = str(c["ChecksNum"])
                rows.append({
                    **base_row,
                    "ItemType": "CHECK",
                    "Num": c["ChecksNum"],
                    "Text": check_dict_texts[key] if key in check_dict_texts else c.get("Text", ""),
                    "Comment": c["Comment"],
                    "Result": c["Result"]
                })
            continue

        if entity == "barrier":
            for b in barriers:
                key = str(b["BarriersNum"])
                rows.append({
                    **base_row,
                    "ItemType": "BARRIER",
                    "Num": b["BarriersNum"],
                    "Text": barrier_dict_texts[key] if key in barrier_dict_texts else b.get("Text", ""),
                    "Comment": b["Comment"],
                    "Result": b["Result"]
                })
            continue

        for c in checks:
            key = str(c["ChecksNum"])
            rows.append({
                **base_row,
                "ItemType": "CHECK", "Num": c["ChecksNum"],
                "Text": check_dict_texts[key] if key in check_dict_texts else c.get("Text", ""),
                "Comment": c["Comment"], "Result": c["Result"]
            })
        for b in barriers:
            key = str(b["BarriersNum"])
            rows.append({
                "DB_KEY": base["DB_KEY"], "Id": base["Id"],
                "Lpc": base.get("Lpc", ""), "LpcText": base.get("LpcText", ""),
                "Profession": base.get("Profession", ""), "ProfessionText": base.get("ProfessionText", ""),
                "DateCheck": base["DateCheck"],
                "ItemType": "BARRIER", "Num": b["BarriersNum"],
                "Text": barrier_dict_texts[key] if key in barrier_dict_texts else b.get("Text", ""),
                "Comment": b["Comment"], "Result": b["Result"]
            })
    if len(rows) > export_limit:
        return _err(400, "EXPORT_LIMIT_EXCEEDED", "Export exceeds configured limit")
    return odata_collection(rows)
