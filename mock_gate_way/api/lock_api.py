from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from database import get_db
from services.lock_service import LockService

router = APIRouter(prefix="/lock", tags=["Lock"])


def _param(v, fallback):
    return v if v not in (None, "") else fallback


@router.post("/acquire")
def acquire(
    object_uuid: str | None = None,
    session_guid: str | None = None,
    uname: str | None = None,
    iv_steal_from: str | None = None,
    pcct_uuid: str | None = None,
    user_id: str | None = None,
    db: Session = Depends(get_db),
):
    obj = _param(object_uuid, pcct_uuid)
    session = _param(session_guid, user_id)
    user = _param(uname, user_id)

    if not obj or not session or not user:
        raise HTTPException(status_code=400, detail="MISSING_LOCK_PARAMS")

    result = LockService.acquire(db, obj, session, user, iv_steal_from)
    if not result["success"]:
        raise HTTPException(status_code=409, detail=result)
    return result


@router.post("/heartbeat")
def heartbeat(
    object_uuid: str | None = None,
    session_guid: str | None = None,
    pcct_uuid: str | None = None,
    user_id: str | None = None,
    db: Session = Depends(get_db),
):
    obj = _param(object_uuid, pcct_uuid)
    session = _param(session_guid, user_id)
    if not obj or not session:
        raise HTTPException(status_code=400, detail="MISSING_LOCK_PARAMS")

    try:
        return LockService.heartbeat(db, obj, session)
    except ValueError as exc:
        code = 410 if str(exc) == "LOCK_EXPIRED" else 409
        raise HTTPException(status_code=code, detail=str(exc)) from exc


@router.post("/release")
def release(
    object_uuid: str | None = None,
    session_guid: str | None = None,
    iv_try_save: bool = False,
    iv_payload: dict | None = None,
    pcct_uuid: str | None = None,
    user_id: str | None = None,
    db: Session = Depends(get_db),
):
    obj = _param(object_uuid, pcct_uuid)
    session = _param(session_guid, user_id)
    if not obj or not session:
        raise HTTPException(status_code=400, detail="MISSING_LOCK_PARAMS")

    return LockService.release(db, obj, session, iv_try_save, iv_payload)
