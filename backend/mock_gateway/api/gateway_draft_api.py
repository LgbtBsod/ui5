"""ChecklistRootPreparationAction / ChecklistRootActivationAction / ChecklistRootDiscardAction
- the manual Fiori draft FunctionImport triad for ChecklistRootSet. Additive and parallel
to the existing session-lock edit flow in gateway_lock_api.py / gateway_save_api.py;
nothing here touches LockEntry/LockService. Business logic lives in services.draft_service
(DraftService), following this codebase's api/services split - route bodies stay thin."""
from fastapi import APIRouter, Depends, Query, Request, Response
from sqlalchemy.orm import Session

from database import get_db
from utils.odata import SERVICE_ROOT
from utils.odata_response import odata_entity
from api.gateway_core import _err, _load_root_or_error, _require_create_permission, _require_permission
from api.gateway_serializers import _to_root, _to_root_draft
from services.draft_service import DraftService

router = APIRouter(tags=["GatewayCanonical"])


@router.post(f"{SERVICE_ROOT}/ChecklistRootPreparationAction")
def checklist_root_preparation_action(
    request: Request,
    active_uuid: str = Query("", alias="ActiveUUID"),
    db: Session = Depends(get_db),
):
    active_uuid = str(active_uuid or "").strip()
    is_create = not active_uuid or set(active_uuid.replace("-", "")) <= {"0"}
    if is_create:
        if (err := _require_create_permission(db, request)):
            return err
    else:
        existing_root, load_err = _load_root_or_error(db, active_uuid)
        if load_err:
            return load_err
        if (err := _require_permission(db, request, existing_root, "edit")):
            return err

    draft = DraftService.prepare(db, active_uuid)
    if draft is None:
        return _err(404, "NOT_FOUND", "Checklist not found")
    return odata_entity(_to_root_draft(draft, db=db))


@router.post(f"{SERVICE_ROOT}/ChecklistRootActivationAction")
def checklist_root_activation_action(
    draft_uuid: str = Query(..., alias="DraftUUID"),
    db: Session = Depends(get_db),
):
    root = DraftService.activate(db, draft_uuid)
    if root is None:
        return _err(404, "NOT_FOUND", "Draft not found")
    return odata_entity(_to_root(root, db=db))


@router.post(f"{SERVICE_ROOT}/ChecklistRootDiscardAction")
def checklist_root_discard_action(
    draft_uuid: str = Query(..., alias="DraftUUID"),
    db: Session = Depends(get_db),
):
    outcome, root = DraftService.discard(db, draft_uuid)
    if outcome == "not_found":
        return _err(404, "NOT_FOUND", "Draft not found")
    if outcome == "create_only":
        return Response(status_code=204)
    return odata_entity(_to_root(root, db=db))
