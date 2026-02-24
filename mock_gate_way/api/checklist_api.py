from fastapi import APIRouter, Depends, HTTPException, Request, Response
from sqlalchemy import func
from sqlalchemy.orm import Session

from database import get_db
from models import ChecklistRoot, Person
from services.checklist_service import ChecklistService
from util.expand_parser import ExpandParser
from util.odata_filter import ODataFilterParser

router = APIRouter(prefix="/checklist", tags=["Checklist"])


@router.post("/")
def create(checklist_id: str, lpc: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ChecklistService.create(db, checklist_id, lpc, user_id)
    except Exception as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc


@router.get("/{root_id}")
def get(root_id: str, request: Request, response: Response, expand: bool = False, db: Session = Depends(get_db)):
    data = ChecklistService.get(db, root_id, expand=expand)
    if not data:
        raise HTTPException(status_code=404, detail="NOT_FOUND")

    etag_date = ChecklistService.calculate_etag(db, root_id)
    if etag_date:
        etag = etag_date.isoformat()
        if request.headers.get("If-None-Match") == etag:
            response.status_code = 304
            return None
        response.headers["ETag"] = etag

    return data


@router.patch("/{root_id}/autosave")
def autosave(root_id: str, user_id: str, payload: dict, force: bool = False, db: Session = Depends(get_db)):
    try:
        return ChecklistService.autosave(db, root_id, user_id, payload, force)
    except Exception as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc


@router.post("/{root_id}/barrier")
def add_barrier(root_id: str, user_id: str, description: str, position: int, db: Session = Depends(get_db)):
    try:
        return ChecklistService.add_barrier(db, root_id, user_id, description, position)
    except Exception as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc


@router.post("/{root_id}/copy")
def copy(root_id: str, user_id: str, db: Session = Depends(get_db)):
    try:
        return ChecklistService.copy(db, root_id, user_id)
    except Exception as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc


@router.get("")
def list_checklists(filter: str = None, expand: str = None, top: int = 50, skip: int = 0, db: Session = Depends(get_db)):
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.is_(False))

    if filter:
        expression = ODataFilterParser.parse(ChecklistRoot, filter)
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


@router.get("/suggest/person")
def suggest_person(search: str = None, checklist_id: str = None, db: Session = Depends(get_db)):
    query = db.query(Person)

    if checklist_id:
        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == checklist_id).first()
        if root and hasattr(root, "date_check") and root.date_check:
            check_date = root.date_check
            query = query.filter(
                Person.begda <= check_date,
                (Person.endda.is_(None)) | (Person.endda >= check_date),
            )

    if search:
        full_name_expr = func.trim(Person.last_name + " " + Person.first_name + " " + Person.middle_name)
        query = query.filter(func.lower(full_name_expr).like(f"%{search.lower()}%"))

    results = query.limit(20).all()
    return {
        "value": [
            {
                "perner": p.perner,
                "fullName": f"{p.last_name} {p.first_name} {p.middle_name}".strip(),
                "position": p.position,
                "orgUnit": p.org_unit,
            }
            for p in results
        ]
    }
