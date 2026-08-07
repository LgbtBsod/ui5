"""CurrentUserSet / ChecklistCreatePermissionSet / ChecklistPermissionSet routes."""

from fastapi import APIRouter, Depends, Query, Request, Response
from sqlalchemy.orm import Session

from api.gateway_core import (
    _apply_order_filter,
    _err,
    _load_root_or_error,
    _normalize_root_filter_aliases,
)
from api.gateway_serializers import _to_create_permission, _to_current_user, _to_permission
from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistRoot
from services.current_user_service import CurrentUserService
from utils.odata import SERVICE_ROOT, odata_payload
from utils.odata_response import odata_collection, odata_entity

router = APIRouter(tags=["GatewayCanonical"])


@router.get(f"{SERVICE_ROOT}/CurrentUserSet")
def current_user_set(request: Request, response: Response, db: Session = Depends(get_db)):
    profile = CurrentUserService.resolve_profile(db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_collection([_to_current_user(profile)])


@router.get(f"{SERVICE_ROOT}/CurrentUserSet({{entity_key}})")
def current_user_entity(entity_key: str, request: Request, response: Response, db: Session = Depends(get_db)):
    cleaned = str(entity_key or "").strip()
    if cleaned.startswith("Key="):
        cleaned = cleaned.split("=", 1)[1]
    cleaned = cleaned.strip("'\"")
    if cleaned and cleaned.upper() != "CURRENT":
        return _err(404, "NOT_FOUND", "Current user not found")
    profile = CurrentUserService.resolve_profile(db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_entity(_to_current_user(profile))


@router.get(f"{SERVICE_ROOT}/ChecklistCreatePermissionSet")
def checklist_create_permission_set(request: Request, response: Response, db: Session = Depends(get_db)):
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_collection([_to_create_permission(resolved_uname, db=db)])


@router.get(f"{SERVICE_ROOT}/ChecklistCreatePermissionSet({{entity_key}})")
def checklist_create_permission_entity(
    entity_key: str, request: Request, response: Response, db: Session = Depends(get_db)
):
    cleaned = str(entity_key or "").strip()
    if cleaned.startswith("DB_KEY=") or cleaned.startswith("Key="):
        cleaned = cleaned.split("=", 1)[1]
    cleaned = cleaned.strip("'\"")
    if cleaned and cleaned.upper() != "CURRENT":
        return _err(404, "NOT_FOUND", "Create permission not found")
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_entity(_to_create_permission(resolved_uname, db=db))


@router.get(f"{SERVICE_ROOT}/ChecklistPermissionSet")
def checklist_permission_set(
    request: Request,
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    filter_expr = _normalize_root_filter_aliases(filter)
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"DB_KEY": "id"},
        filter_expr,
        orderby,
        top,
        skip,
    )
    payload = [_to_permission(row, resolved_uname, db=db) for row in rows]
    return odata_payload(payload, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({{entity_key}})")
def checklist_permission_entity(entity_key: str, request: Request, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    return odata_entity(_to_permission(root, resolved_uname, db=db))
