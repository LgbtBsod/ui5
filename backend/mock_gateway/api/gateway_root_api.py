"""ChecklistSearchSet / ChecklistRootSet / ChecklistBasicInfoSet routes."""

import uuid

from fastapi import APIRouter, Depends, Query, Request, Response
from sqlalchemy.orm import Session

from api.gateway_core import (
    _agg_changed_on,
    _apply_order_filter,
    _apply_select,
    _err,
    _load_root_or_draft_or_error,
    _load_root_or_error,
    _reject_expand,
    _require_create_permission,
    _require_permission,
)
from api.gateway_operations import _apply_root_payload
from api.gateway_serializers import (
    ROOT_MAP,
    SEARCH_MAP,
    _apply_orderby_rows,
    _to_basic,
    _to_root,
    _to_root_draft,
    _to_search,
)
from api.gateway_validators import (
    _apply_basic_payload,
    _next_checklist_id,
    _normalize_basic_payload,
    _normalize_status_input,
    _pick_text,
)
from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistRoot
from services.analytics_service import AnalyticsService
from services.current_user_service import CurrentUserService
from services.lock_service import LockService
from utils.filter_parser import FilterParser
from utils.odata import SERVICE_ROOT, format_entity_etag, odata_payload
from utils.odata_query import parse_paging, with_inlinecount
from utils.odata_response import odata_collection, odata_entity
from utils.sap_message import build_sap_message
from utils.time import now_utc

router = APIRouter(tags=["GatewayCanonical"])


@router.get(f"{SERVICE_ROOT}/ChecklistSearchSet")
def checklist_search_set(
    filter_expr: str | None = Query(None, alias="$filter"),
    expand: str | None = Query(None, alias="$expand"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    select: str | None = Query(None, alias="$select"),
    db: Session = Depends(get_db),
):
    if err := _reject_expand(expand):
        return err
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True))
    expr = FilterParser.parse(ChecklistRoot, filter_expr, field_map=SEARCH_MAP)
    if expr is not None:
        query = query.filter(expr)
    roots = query.all()
    mapped = [_to_search(r, db=db) for r in roots]
    mapped = _apply_orderby_rows(mapped, orderby)
    top, skip = parse_paging(top, skip, DEFAULT_PAGE_SIZE)
    total = len(mapped)
    paged = mapped[skip : skip + top]
    return odata_collection([_apply_select(r, select) for r in paged], with_inlinecount(inlinecount, total))


@router.get(f"{SERVICE_ROOT}/ChecklistSearchSet/$count")
def checklist_search_count(filter_expr: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True))
    expr = FilterParser.parse(ChecklistRoot, filter_expr, field_map=SEARCH_MAP)
    if expr is not None:
        query = query.filter(expr)
    return Response(content=str(query.count()), media_type="text/plain")


@router.get(f"{SERVICE_ROOT}/ChecklistRootSet")
def checklist_root_set(
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    select: str | None = Query(None, alias="$select"),
    db: Session = Depends(get_db),
):
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        ROOT_MAP,
        filter,
        orderby,
        top,
        skip,
    )
    return odata_payload(
        [_apply_select(_to_root(r, db=db), select) for r in rows], total if inlinecount == "allpages" else None
    )


@router.get(f"{SERVICE_ROOT}/ChecklistRootSet({{entity_key}})")
def checklist_root_entity(entity_key: str, response: Response, db: Session = Depends(get_db)):
    kind, obj, err = _load_root_or_draft_or_error(db, entity_key)
    if err:
        return err
    if kind == "draft":
        return odata_entity(_to_root_draft(obj, db=db))
    root = obj
    response.headers["ETag"] = format_entity_etag(_agg_changed_on(root), root.version_number)
    return odata_entity(_to_root(root, db=db))


def _build_new_root(
    db: Session, *, checklist_id: str, lpc: str, status: str, user_name: str, basic_values: dict
) -> ChecklistRoot:
    """Shared ChecklistRoot construction for checklist_root_create/create_checklist - these
    independently built the same 8-field ChecklistRoot(...) plus _apply_basic_payload/
    changed_on assignment, only differing in how they parse their own request shape into
    these arguments."""
    root = ChecklistRoot(
        id=str(uuid.uuid4()),
        checklist_id=checklist_id or _next_checklist_id(db),
        lpc=lpc,
        status=_normalize_status_input(status),
        integration_flag=False,
        created_by=user_name,
        changed_by=user_name,
        version_number=1,
    )
    _apply_basic_payload(root, basic_values)
    root.changed_on = now_utc()
    return root


@router.post(f"{SERVICE_ROOT}/ChecklistRootSet")
def checklist_root_create(payload: dict, response: Response, request: Request, db: Session = Depends(get_db)):
    if err := _require_create_permission(db, request):
        return err
    basic_values = _normalize_basic_payload(payload, db)
    requested_id = _pick_text(payload, "RequestId", "Id", "checklist_id")
    requested_status = _pick_text(payload, "Status", "status") or "DRAFT"
    user_name = CurrentUserService.resolve_uname(db=db)

    root = _build_new_root(
        db,
        checklist_id=requested_id,
        lpc=str(payload.get("lpc") or basic_values.get("lpc") or ""),
        status=requested_status,
        user_name=user_name,
        basic_values=basic_values,
    )
    db.add(root)
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    response.headers["sap-message"] = build_sap_message("Checklist created", "success", code="CREATED")
    return odata_entity(_to_root(root, db=db))


@router.patch(f"{SERVICE_ROOT}/ChecklistRootSet({{entity_key}})")
def checklist_root_update(
    entity_key: str, payload: dict, response: Response, request: Request, db: Session = Depends(get_db)
):
    kind, obj, err = _load_root_or_draft_or_error(db, entity_key)
    if err:
        return err
    if kind == "draft":
        draft = obj
        if draft.active_id:
            active_for_permission = db.query(ChecklistRoot).filter(ChecklistRoot.id == draft.active_id).first()
            if active_for_permission and (err := _require_permission(db, request, active_for_permission, "edit")):
                return err
        elif err := _require_create_permission(db, request):
            return err
        _apply_root_payload(draft, payload)
        _apply_basic_payload(draft, _normalize_basic_payload(payload, db))
        draft.changed_by = CurrentUserService.resolve_uname(db=db) or draft.changed_by or "ANON"
        draft.changed_on = now_utc()
        db.commit()
        response.headers["sap-message"] = build_sap_message("Draft updated", "success", code="SAVED")
        return odata_entity(_to_root_draft(draft, db=db))

    root = obj
    if err := _require_permission(db, request, root, "edit"):
        return err
    session_guid = str(payload.get("SessionGuid") or payload.get("session_guid") or "").strip()
    if session_guid:
        try:
            LockService.validate_session_lock(db, root.id, session_guid)
        except ValueError as ex:
            if str(ex) in {"LOCK_MISSING", "LOCK_NOT_OWNED_BY_SESSION"}:
                return _err(409, str(ex), "Active lock for session is required")
            return _err(409, "LOCK_EXPIRED", "Lock expired")

    _apply_root_payload(root, payload)
    _apply_basic_payload(root, _normalize_basic_payload(payload, db))
    root.changed_by = CurrentUserService.resolve_uname(db=db) or root.changed_by or "ANON"
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    response.headers["sap-message"] = build_sap_message("Checklist updated", "success", code="SAVED")
    return odata_entity(_to_root(root, db=db))


@router.delete(f"{SERVICE_ROOT}/ChecklistRootSet({{entity_key}})")
def checklist_root_delete(entity_key: str, request: Request, db: Session = Depends(get_db)):
    kind, obj, err = _load_root_or_draft_or_error(db, entity_key)
    if err:
        return err
    if kind == "draft":
        db.delete(obj)
        db.commit()
        return Response(status_code=204)

    root = obj
    if err := _require_permission(db, request, root, "delete"):
        return err
    root.is_deleted = True
    root.changed_on = now_utc()
    db.commit()
    AnalyticsService.mark_dirty()
    return Response(status_code=204)


@router.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet")
def checklist_basic_info_set(
    filter: str | None = Query(None, alias="$filter"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"DB_KEY": "id"},
        filter,
        None,
        top,
        skip,
    )
    return odata_payload([_to_basic(r) for r in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet({{entity_key}})")
def checklist_basic_info_entity(entity_key: str, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    return odata_entity(_to_basic(root))
