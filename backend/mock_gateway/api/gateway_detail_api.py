"""ChecklistCheckSet / ChecklistBarrierSet routes."""

import re
import uuid

from fastapi import APIRouter, Depends, Query, Request, Response
from sqlalchemy.orm import Session

from api.gateway_core import (
    _apply_order_filter,
    _boundary_parent_key,
    _entity_key,
    _err,
    _load_root_or_error,
    _reject_expand,
    _require_permission,
)
from api.gateway_detail_kind import _BARRIER_KIND, _CHECK_KIND, _DetailKind
from database import get_db
from models import ChecklistRoot
from utils.odata import SERVICE_ROOT, odata_payload
from utils.odata_response import odata_entity
from utils.time import now_utc

router = APIRouter(tags=["GatewayCanonical"])


def _normalize_rest_payload_for_create(payload: dict, kind: _DetailKind) -> dict:
    return {
        "text": str(payload.get("Text") or "").strip(),
        "comment": str(payload.get("Comment") or "").strip(),
        "result": bool(payload.get("Result")),
        "position": int(payload.get(kind.number_field) or 0) or 1,
    }


def _normalize_rest_payload_for_update(payload: dict, kind: _DetailKind) -> dict:
    # Result is always overwritten (falls back to False, not left untouched) to match
    # the pre-existing REST PATCH contract: a PATCH without "Result" clears it to FAIL/inactive.
    normalized = {"result": bool(payload.get("Result"))}
    if payload.get("Text") is not None:
        normalized["text"] = str(payload.get("Text") or "").strip()
    if payload.get("Comment") is not None:
        normalized["comment"] = str(payload.get("Comment") or "").strip()
    if payload.get(kind.number_field) is not None:
        normalized["position"] = int(payload.get(kind.number_field) or 0) or 1
    return normalized


def _detail_list(db: Session, kind: _DetailKind, filter_expr: str | None, top: int, skip: int, inlinecount: str | None):
    """Shared GET-collection body for ChecklistCheckSet/ChecklistBarrierSet."""
    filter_expr = re.sub(r"\bRootId\b", "PARENT_KEY", str(filter_expr or ""), flags=re.IGNORECASE)
    filter_expr = re.sub(r"\bRootKey\b", "PARENT_KEY", filter_expr, flags=re.IGNORECASE)
    rows, total = _apply_order_filter(db.query(kind.model), kind.model, kind.field_map, filter_expr, None, top, skip)
    return odata_payload([kind.serialize(r) for r in rows], total if inlinecount == "allpages" else None)


def _detail_create(db: Session, request: Request, payload: dict, kind: _DetailKind):
    """Shared POST body for ChecklistCheckSet/ChecklistBarrierSet."""
    root, err = _load_root_or_error(db, _boundary_parent_key(payload))
    if err:
        return err
    if err := _require_permission(db, request, root, "edit"):
        return err
    row = kind.model(
        id=str(uuid.uuid4()),
        root_id=root.id,
        created_on=now_utc(),
        changed_on=now_utc(),
        **kind.to_kwargs(_normalize_rest_payload_for_create(payload, kind)),
    )
    db.add(row)
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    return odata_entity(kind.serialize(row))


def _detail_update(db: Session, request: Request, entity_key: str, payload: dict, kind: _DetailKind):
    """Shared PATCH body for ChecklistCheckSet({key})/ChecklistBarrierSet({key})."""
    key = _entity_key(entity_key)
    row = db.query(kind.model).filter(kind.model.id == key).first()
    if not row:
        return _err(404, "NOT_FOUND", kind.not_found_message)
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == row.root_id).first()
    if root and (err := _require_permission(db, request, root, "edit")):
        return err
    kind.apply_update(row, _normalize_rest_payload_for_update(payload, kind))
    row.changed_on = now_utc()
    if root:
        root.changed_on = now_utc()
        root.version_number = int(root.version_number or 0) + 1
    db.commit()
    return odata_entity(kind.serialize(row))


def _detail_delete(db: Session, request: Request, entity_key: str, kind: _DetailKind):
    """Shared DELETE body for ChecklistCheckSet({key})/ChecklistBarrierSet({key})."""
    key = _entity_key(entity_key)
    row = db.query(kind.model).filter(kind.model.id == key).first()
    if not row:
        return _err(404, "NOT_FOUND", kind.not_found_message)
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == row.root_id).first()
    if root and (err := _require_permission(db, request, root, "edit")):
        return err
    db.delete(row)
    if root:
        root.changed_on = now_utc()
        root.version_number = int(root.version_number or 0) + 1
    db.commit()
    return Response(status_code=204)


@router.get(f"{SERVICE_ROOT}/ChecklistCheckSet")
def checklist_check_set(
    filter: str | None = Query(None, alias="$filter"),
    expand: str | None = Query(None, alias="$expand"),
    top: int = Query(20, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    if err := _reject_expand(expand):
        return err
    return _detail_list(db, _CHECK_KIND, filter, top, skip, inlinecount)


@router.post(f"{SERVICE_ROOT}/ChecklistCheckSet")
def checklist_check_create(payload: dict, request: Request, db: Session = Depends(get_db)):
    return _detail_create(db, request, payload, _CHECK_KIND)


@router.patch(f"{SERVICE_ROOT}/ChecklistCheckSet({{entity_key}})")
def checklist_check_update(entity_key: str, payload: dict, request: Request, db: Session = Depends(get_db)):
    return _detail_update(db, request, entity_key, payload, _CHECK_KIND)


@router.delete(f"{SERVICE_ROOT}/ChecklistCheckSet({{entity_key}})")
def checklist_check_delete(entity_key: str, request: Request, db: Session = Depends(get_db)):
    return _detail_delete(db, request, entity_key, _CHECK_KIND)


@router.get(f"{SERVICE_ROOT}/ChecklistBarrierSet")
def checklist_barrier_set(
    filter: str | None = Query(None, alias="$filter"),
    expand: str | None = Query(None, alias="$expand"),
    top: int = Query(20, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    if err := _reject_expand(expand):
        return err
    return _detail_list(db, _BARRIER_KIND, filter, top, skip, inlinecount)


@router.post(f"{SERVICE_ROOT}/ChecklistBarrierSet")
def checklist_barrier_create(payload: dict, request: Request, db: Session = Depends(get_db)):
    return _detail_create(db, request, payload, _BARRIER_KIND)


@router.patch(f"{SERVICE_ROOT}/ChecklistBarrierSet({{entity_key}})")
def checklist_barrier_update(entity_key: str, payload: dict, request: Request, db: Session = Depends(get_db)):
    return _detail_update(db, request, entity_key, payload, _BARRIER_KIND)


@router.delete(f"{SERVICE_ROOT}/ChecklistBarrierSet({{entity_key}})")
def checklist_barrier_delete(entity_key: str, request: Request, db: Session = Depends(get_db)):
    return _detail_delete(db, request, entity_key, _BARRIER_KIND)
