# api/lock_api.py

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from database import get_db
from services.lock_service import LockService

router = APIRouter(prefix="/lock", tags=["Lock"])


@router.post("/acquire")
def acquire(pcct_uuid: str, user_id: str, db: Session = Depends(get_db)):
    try:
        lock = LockService.acquire(db, pcct_uuid, user_id)
        return {"status": "ACQUIRED", "expires_at": lock.expires_at}
    except Exception as e:
        raise HTTPException(status_code=409, detail=str(e))


@router.post("/heartbeat")
def heartbeat(pcct_uuid: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return LockService.heartbeat(db, pcct_uuid, user_id)
    except Exception as e:
        raise HTTPException(status_code=409, detail=str(e))


@router.post("/release")
def release(pcct_uuid: str, user_id: str, db: Session = Depends(get_db)):
    try:
        LockService.release(db, pcct_uuid, user_id)
        return {"status": "RELEASED"}
    except Exception as e:
        raise HTTPException(status_code=409, detail=str(e))

@router.post("/steal")
def steal(pcct_uuid: str, user_id: str, db: Session = Depends(get_db)):
    try:
        lock = LockService.steal(db, pcct_uuid, user_id)
        return {"status": "STOLEN", "expires_at": lock.expires_at}
    except Exception as e:
        raise HTTPException(status_code=409, detail=str(e))