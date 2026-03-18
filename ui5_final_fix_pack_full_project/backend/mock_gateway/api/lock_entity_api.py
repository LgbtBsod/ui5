from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from database import get_db
from services.lock_service import LockService

router = APIRouter(tags=["LockEntity"])


@router.get("/LockStatusSet")
def lock_status_set(object_uuid: str, session_guid: str, db: Session = Depends(get_db)):
    try:
        status = LockService.status(db, object_uuid, session_guid)
        return {"d": {"results": [status]}}
    except ValueError as exc:
        message = str(exc)
        if message == "LOCK_EXPIRED":
            raise HTTPException(status_code=410, detail=message) from exc
        raise HTTPException(status_code=409, detail=message) from exc
