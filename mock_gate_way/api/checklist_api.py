from fastapi import APIRouter, Depends, Header, HTTPException, Request, Response
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistRoot
from services.checklist_service import ChecklistService
from util.expand_parser import ExpandParser
from utils.filter_parser import FilterParser

router = APIRouter(prefix="/checklist", tags=["Checklist"])


@router.post("/")
def create(checklist_id: str, lpc: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ChecklistService.create(db, checklist_id, lpc, user_id)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc


@router.get("/{root_id}")
def get(root_id: str, request: Request, response: Response, expand: bool = False, db: Session = Depends(get_db)):
    data = ChecklistService.get(db, root_id, expand=expand)
    if not data:
        raise HTTPException(status_code=404, detail="NOT_FOUND")

    etag = ChecklistService.get_etag_value(db, root_id)
    if etag:
        if request.headers.get("If-None-Match") == etag:
            response.status_code = 304
            return None
        response.headers["ETag"] = etag

    return data


@router.patch("/{root_id}")
def update_with_etag(
    root_id: str,
    payload: dict,
    user_id: str,
    if_match: str | None = Header(default=None, alias="If-Match"),
    db: Session = Depends(get_db),
):
    try:
        updated = ChecklistService.update_with_etag(db, root_id, user_id, payload, if_match)
        return {"status": "updated", "id": updated.id}
    except ValueError as exc:
        message = str(exc)
        if message == "NOT_FOUND":
            raise HTTPException(status_code=404, detail=message) from exc
        if message == "PRECONDITION_REQUIRED":
            raise HTTPException(status_code=428, detail=message) from exc
        if message == "ETAG_MISMATCH":
            raise HTTPException(status_code=412, detail=message) from exc
        raise HTTPException(status_code=409, detail=message) from exc


@router.patch("/{root_id}/autosave")
def autosave(root_id: str, user_id: str, payload: dict, force: bool = False, db: Session = Depends(get_db)):
    try:
        return ChecklistService.autosave(db, root_id, user_id, payload, force)
    except ValueError as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc


@router.post("/{root_id}/barrier")
def add_barrier(root_id: str, user_id: str, description: str, position: int, db: Session = Depends(get_db)):
    try:
        return ChecklistService.add_barrier(db, root_id, user_id, description, position)
    except ValueError as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc


@router.post("/{root_id}/copy")
def copy(root_id: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ChecklistService.copy(db, root_id, user_id)
    except ValueError as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc


@router.get("")
def list_checklists(filter: str = None, expand: str = None, top: int = DEFAULT_PAGE_SIZE, skip: int = 0, db: Session = Depends(get_db)):
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.is_(False))

    expression = FilterParser.parse(ChecklistRoot, filter)
    if expression is not None:
        query = query.filter(expression)

    if expand:
        query = ExpandParser.apply(query, ChecklistRoot, expand)

    total = query.count()
    roots = query.offset(skip).limit(top).all()

    value = [
        {
            "id": item.id,
            "checklist_id": item.checklist_id,
            "lpc": item.lpc,
            "status": item.status,
            "changed_on": item.changed_on,
        }
        for item in roots
    ]
    return {"value": value, "count": total}
