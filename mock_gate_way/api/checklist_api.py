from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from database import get_db
from services.checklist_service import ChecklistService

from utils.odata_filter import ODataFilterParser

router = APIRouter(prefix="/checklist", tags=["Checklist"])


@router.post("/")
def create(checklist_id: str, lpc: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ChecklistService.create(db, checklist_id, lpc, user_id)
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))


@router.get("/{root_id}")
def get(root_id: str, expand: bool = False, db: Session = Depends(get_db)):
    data = ChecklistService.get(db, root_id, expand)
    if not data:
        raise HTTPException(status_code=404, detail="NOT_FOUND")
    return data


@router.patch("/{root_id}/autosave")
def autosave(
    root_id: str,
    user_id: str,
    payload: dict,
    force: bool = False,
    db: Session = Depends(get_db)
):
    try:
        return ChecklistService.autosave(db, root_id, user_id, payload, force)
    except Exception as e:
        raise HTTPException(status_code=409, detail=str(e))


@router.post("/{root_id}/barrier")
def add_barrier(
    root_id: str,
    user_id: str,
    description: str,
    position: int,
    db: Session = Depends(get_db)
):
    try:
        return ChecklistService.add_barrier(db, root_id, user_id, description, position)
    except Exception as e:
        raise HTTPException(status_code=409, detail=str(e))


@router.post("/{root_id}/copy")
def copy(root_id: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ChecklistService.copy(db, root_id, user_id)
    except Exception as e:
        raise HTTPException(status_code=409, detail=str(e))

etag_date = ChecklistService.calculate_etag(db, root_id)

if etag_date:
    etag = etag_date.isoformat()
else:
    etag = None

client_etag = request.headers.get("If-None-Match")

if client_etag and client_etag == etag:
    response.status_code = 304
    return

if etag:
    response.headers["ETag"] = etag

@router.get("")
def list_checklists(
    request: Request,
    response: Response,
    filter: str = None,
    top: int = 50,
    skip: int = 0,
    db: Session = Depends(get_db)
):
    query = db.query(ChecklistRoot)

    if filter:
        expression = ODataFilterParser.parse(ChecklistRoot, filter)
        if expression is not None:
            query = query.filter(expression)

    total = query.count()

    data = query.offset(skip).limit(top).all()

    result = []
    for r in data:
        result.append({
            "id": r.id,
            "title": r.title,
            "lpc_level": r.lpc_level,
            "changed_on": r.changed_on
        })

    return {
        "value": result,
        "count": total
    }