"""AutoSave / CreateChecklist / CopyChecklist / SaveChanges / SetChecklistStatus /
GetHierarchy / ReportExport routes.

Refactored to use SaveService and ReportExportService for business logic (SRP compliance).
"""

import json
import uuid
from datetime import datetime, timezone
from typing import Any

from fastapi import APIRouter, Depends, Header, Query, Request, Response
from sqlalchemy.orm import Session

from api.gateway_core import (
    _agg_changed_on,
    _boundary_root_key,
    _entity_key,
    _err,
    _if_match_check,
    _load_root_or_error,
    _merge_query_and_payload,
    _optimistic_check,
    _parse_odata_datetime,
    _require_create_permission,
    _require_permission,
    _validate_status_change,
)
from api.gateway_detail_kind import _BARRIER_KIND, _CHECK_KIND
from api.gateway_mutations import (
    _apply_save_detail_rows,
    _apply_save_root,
    _save_request_root,
    _save_request_root_key,
    _save_request_session,
)
from api.gateway_operations import _apply_save_attachments, _hex, _normalize_hex_key, _replace_detail_rows
from api.gateway_root_api import _build_new_root
from api.gateway_serializers import (
    _normalize_export_limit,
    _normalize_export_search_contract,
    _search_contract_matches,
    _to_barrier,
    _to_check,
    _to_root,
    _to_search,
)
from api.gateway_validators import _normalize_basic_payload, _normalize_status_input, _pick_text, _status_external
from config import LOCK_TTL
from database import get_db
from models import ChecklistRoot, DictionaryItem
from services.analytics_service import AnalyticsService
from services.checklist_service import ChecklistService
from services.current_user_service import CurrentUserService
from services.hierarchy_service import HierarchyService
from services.lock_service import LockService
from services.report_export_service import ReportExportService
from services.save_service import SaveResult, SaveService
from utils.odata import SERVICE_ROOT, format_datetime
from utils.odata_response import odata_collection, odata_entity
from utils.sap_message import build_sap_message
from utils.time import now_utc

router = APIRouter(tags=["GatewayCanonical"])


@router.post(f"{SERVICE_ROOT}/AutoSave")
def auto_save(
    payload: dict,
    response: Response,
    request: Request,
    if_match: str | None = Header(None, alias="If-Match"),
    db: Session = Depends(get_db),
):
    """Handle AutoSave operation for checklist.

    Uses SaveService for business logic (SRP, DIP compliance).

    Args:
        payload: Request payload containing checklist data
        response: FastAPI response object
        request: FastAPI request object
        if_match: ETag for optimistic concurrency
        db: Database session

    Returns:
        JSONResponse with save confirmation and lock status
    """
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    root, err = _load_root_or_error(db, _save_request_root_key(payload))
    if err:
        return err
    if err := _require_permission(db, request, root, "edit"):
        return err
    session_guid = _save_request_session(payload)
    if not session_guid:
        return _err(400, "VALIDATION_ERROR", "SessionGuid is required")

    # Use SaveService for lock validation (DIP, SRP)
    lock_error = SaveService.validate_lock_or_error(db, root.id, session_guid)
    if lock_error:
        return _err(409, lock_error.error_code or "LOCK_ERROR", lock_error.error_message or "Lock validation failed")

    # Use SaveService for save operation (SRP, DRY)
    result = SaveService.apply_save_and_commit(
        db=db,
        root=root,
        payload_body=body,
        check_kind=_CHECK_KIND,
        barrier_kind=_BARRIER_KIND,
        session_guid=session_guid,
    )

    SaveService.build_success_response(result, response, "Autosave completed", "SAVED")
    return odata_entity(result.to_dict())


@router.post(f"{SERVICE_ROOT}/CreateChecklist")
def create_checklist(payload: dict, response: Response, request: Request, db: Session = Depends(get_db)):
    if err := _require_create_permission(db, request):
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
async def copy_checklist(
    request: Request,
    root_id: str | None = Query(None, alias="DB_KEY"),
    session_guid: str | None = Query(None, alias="SessionGuid"),
    payload: dict | None = None,
    db: Session = Depends(get_db),
):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _boundary_root_key(
        merged, root_id, merged.get("source_parent_key"), merged.get("SourceParentKey"), merged.get("SourceUuid")
    )
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")

    source_root, err = _load_root_or_error(db, resolved_root_id)
    if err:
        return err
    if err := _require_permission(db, request, source_root, "view"):
        return err
    if err := _require_create_permission(db, request):
        return err

    try:
        copied_root = ChecklistService.copy(db, source_root.id, str(resolved_uname))
        lock_result = LockService.acquire(
            db, copied_root.id, str(resolved_session_guid).strip(), str(resolved_uname), None
        )
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
def save_changes(
    payload: dict,
    response: Response,
    request: Request,
    if_match: str | None = Header(None, alias="If-Match"),
    db: Session = Depends(get_db),
):
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    root, err = _load_root_or_error(db, _save_request_root_key(payload))
    if err:
        return err
    if err := _require_permission(db, request, root, "edit"):
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
            return _err(
                400, "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN", "Attachment upload must use media stream endpoint"
            )
        return _err(400, "INVALID_ATTACHMENT_PAYLOAD", "Attachment payload is invalid")
    except RuntimeError as ex:
        db.rollback()
        return ex.args[0]
    active_lock = LockService.active_lock(db, root.id)
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
    active_lock = LockService.active_lock(db, root.id)
    request_id = str(uuid.uuid4())
    response.headers["sap-message"] = build_sap_message("Checklist updated", "success", code="SAVED")
    return odata_entity(
        {
            "db_key": _hex(root.id),
            "changed_on": format_datetime(root.changed_on),
            "version_number": int(root.version_number or 0),
            "code": "LOCK_OK",
            "reason_code": "SAVED",
            "lock_refreshed": True,
            "lock_expires_at": format_datetime(LockService.lock_expires_at(active_lock)) if active_lock else "",
            "server_now": format_datetime(now_utc()),
            "request_id": request_id,
        }
    )


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
    resolved_new_status = (
        new_status_q
        or merged.get("NewStatus")
        or merged.get("new_status")
        or merged.get("Status")
        or merged.get("status")
        or ""
    )
    resolved_client_agg = client_agg_q or merged.get("ClientAggChangedOn") or merged.get("client_agg_changed_on")

    root, err = _load_root_or_error(db, resolved_root_key)
    if err:
        return err
    if err := _require_permission(db, request, root, "edit"):
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
    return odata_entity(
        {
            "DB_KEY": _hex(root.id),
            "Status": _status_external(root.status),
            "AggChangedOn": format_datetime(_agg_changed_on(root)),
            "Message": "Status updated",
            "ReasonCode": "STATUS_SET",
        }
    )


@router.get(f"{SERVICE_ROOT}/GetHierarchy")
@router.post(f"{SERVICE_ROOT}/GetHierarchy")
async def get_hierarchy(
    request: Request,
    date_check: str | None = Query(None, alias="DateCheck"),
    method: str | None = Query(None, alias="Method"),
    db: Session = Depends(get_db),
):
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
    return odata_entity(
        {"DateCheck": format_datetime(datetime(dt.year, dt.month, dt.day, tzinfo=timezone.utc)), "results": nodes}
    )


@router.post(f"{SERVICE_ROOT}/ReportExport")
def report_export(payload: dict, db: Session = Depends(get_db)):
    """Handle ReportExport operation.

    Uses ReportExportService for business logic (SRP, DRY compliance).

    Args:
        payload: Request payload containing export parameters
        db: Database session

    Returns:
        OData collection of export rows or error response
    """
    payload = payload if isinstance(payload, dict) else {}
    selection_mode = str(payload.get("SelectionMode") or "").strip().lower()
    entity = str(payload.get("Entity") or "screen").strip().lower() or "screen"
    export_limit = _normalize_export_limit(payload.get("Limit") or payload.get("limit"))
    root_keys = payload.get("RootKeys") or payload.get("DBKeys") or payload.get("keys") or payload.get("ids") or []

    if not root_keys:
        single_key = _boundary_root_key(payload)
        if single_key:
            root_keys = [single_key]

    root_keys = [_normalize_hex_key(value) for value in root_keys if _normalize_hex_key(value)]

    # Use ReportExportService for root retrieval and validation (SRP)
    export_service = ReportExportService(db)

    try:
        roots = export_service.get_roots_for_export(
            root_keys=root_keys if root_keys else None,
            selection_mode=selection_mode,
            search_contract=_normalize_export_search_contract(payload.get("SearchContract")),
            export_limit=export_limit,
        )
    except ValueError as ex:
        return _err(400, "EXPORT_LIMIT_EXCEEDED", str(ex))

    # Use ReportExportService for building export rows (DRY, SRP)
    dictionary_texts = export_service.load_dictionary_texts()
    rows = export_service.build_export_rows(roots=roots, entity_type=entity, dictionary_texts=dictionary_texts)

    if not export_service.validate_export_limit(rows, export_limit):
        return _err(400, "EXPORT_LIMIT_EXCEEDED", "Export exceeds configured limit")

    return odata_collection(rows)
