from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from database import get_db
from services.action_service import ActionService
from services.checklist_service import ChecklistService
from services.lock_service import LockService
from services.hierarchy_service import HierarchyService
from datetime import date as date_type

router = APIRouter(prefix="/actions", tags=["Actions"])


def _error(status: int, exc: ValueError):
    raise HTTPException(status_code=status, detail=str(exc)) from exc


@router.post("/{root_id}/set-status")
def set_status(root_id: str, status: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ActionService.set_status(db, root_id, status, user_id)
    except ValueError as exc:
        _error(404, exc)


@router.post("/SubmitChecklist")
def submit_checklist(root_id: str, user_id: str, db: Session = Depends(get_db)):
    try:
        result = ActionService.set_status(db, root_id, "SUBMITTED", user_id)
        return {"d": result["status"]}
    except ValueError as exc:
        _error(404, exc)


@router.post("/UnlockChecklist")
def unlock_checklist(root_id: str, user_id: str, db: Session = Depends(get_db)):
    try:
        LockService.release(db, root_id, user_id)
        return {"d": True}
    except ValueError as exc:
        _error(409, exc)


@router.post("/LockAcquire")
def lock_acquire(root_id: str, user_id: str, session_guid: str, iv_steal_from: str | None = None, db: Session = Depends(get_db)):
    result = LockService.acquire(db, root_id, session_guid, user_id, iv_steal_from)
    if not result["success"]:
        raise HTTPException(status_code=409, detail=result)
    return {"d": result}


@router.post("/LockHeartbeat")
def lock_heartbeat(root_id: str, session_guid: str, db: Session = Depends(get_db)):
    try:
        return {"d": LockService.heartbeat(db, root_id, session_guid)}
    except ValueError as exc:
        _error(409, exc)


@router.post("/LockStatus")
def lock_status(root_id: str, session_guid: str, db: Session = Depends(get_db)):
    try:
        return {"d": LockService.status(db, root_id, session_guid)}
    except ValueError as exc:
        _error(409, exc)


@router.post("/LockRelease")
def lock_release(root_id: str, session_guid: str, iv_try_save: bool = False, db: Session = Depends(get_db)):
    try:
        return {"d": LockService.release(db, root_id, session_guid, iv_try_save, None)}
    except ValueError as exc:
        _error(409, exc)


@router.post("/SaveChecklist")
def save_checklist(root_id: str, user_id: str, payload: dict, force: bool = False, request_guid: str | None = None, db: Session = Depends(get_db)):
    try:
        result = ChecklistService.save_via_import(db, root_id, user_id, payload, is_autosave=False, force=force, request_guid=request_guid)
        return {"d": result}
    except ValueError as exc:
        _error(409, exc)


@router.post("/AutoSaveChecklist")
def autosave_checklist(root_id: str, user_id: str, payload: dict, force: bool = False, request_guid: str | None = None, db: Session = Depends(get_db)):
    try:
        result = ChecklistService.save_via_import(db, root_id, user_id, payload, is_autosave=True, force=force, request_guid=request_guid)
        return {"d": result}
    except ValueError as exc:
        _error(409, exc)


@router.post("/GetMplHierarchy")
def get_mpl_hierarchy(date: str, db: Session = Depends(get_db)):
    parsed_date = date_type.fromisoformat(date)
    return {"d": {"results": HierarchyService.get_tree(db, parsed_date)}}


@router.post("/export")
def export_report(payload: dict, db: Session = Depends(get_db)):
    s_entity = (payload or {}).get("entity") or "checklist"
    m_filters = (payload or {}).get("filters") or {}
    s_mode = (payload or {}).get("search_mode") or "EXACT"
    return ActionService.export_report(db, s_entity, m_filters, s_mode)
