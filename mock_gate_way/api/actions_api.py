from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from database import get_db
from services.action_service import ActionService
from services.lock_service import LockService

router = APIRouter(prefix="/actions", tags=["Actions"])


@router.post("/{root_id}/set-status")
def set_status(root_id: str, status: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ActionService.set_status(db, root_id, status, user_id)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc


@router.post("/SubmitChecklist")
def submit_checklist(root_id: str, user_id: str, db: Session = Depends(get_db)):
    try:
        result = ActionService.set_status(db, root_id, "SUBMITTED", user_id)
        return {"d": result["status"]}
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc


@router.post("/UnlockChecklist")
def unlock_checklist(root_id: str, user_id: str, db: Session = Depends(get_db)):
    try:
        LockService.release(db, root_id, user_id)
        return {"d": True}
    except ValueError as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc


@router.post("/export")
def export_report(payload: dict, db: Session = Depends(get_db)):
    s_entity = (payload or {}).get("entity") or "checklist"
    m_filters = (payload or {}).get("filters") or {}
    s_mode = (payload or {}).get("search_mode") or "EXACT"
    return ActionService.export_report(db, s_entity, m_filters, s_mode)
