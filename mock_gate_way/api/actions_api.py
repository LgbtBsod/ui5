from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from database import get_db
from services.action_service import ActionService

router = APIRouter(prefix="/actions", tags=["Actions"])


@router.post("/{root_id}/set-status")
def set_status(root_id: str, status: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ActionService.set_status(db, root_id, status, user_id)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
