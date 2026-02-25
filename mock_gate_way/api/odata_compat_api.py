from fastapi import APIRouter, Depends, Query
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistRoot
from util.expand_parser import ExpandParser
from utils.filter_parser import FilterParser

router = APIRouter(tags=["ODataCompatibility"])


def _project_item(item: ChecklistRoot):
    return {
        "id": item.id,
        "checklist_id": item.checklist_id,
        "status": item.status,
        "lpc": item.lpc,
        "date": item.date,
        "equipment": item.equipment,
        "observer_fullname": item.observer_fullname,
        "changed_on": item.changed_on,
        "changed_by": item.changed_by,
        "created_on": item.created_on,
    }


@router.get("/ChecklistRoots")
def checklist_roots(
    filter: str | None = Query(default=None, alias="$filter"),
    expand: str | None = Query(default=None, alias="$expand"),
    top: int = Query(default=DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(default=0, alias="$skip"),
    inlinecount: str | None = Query(default=None, alias="$inlinecount"),
    db: Session = Depends(get_db)
):
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.is_(False))

    expression = FilterParser.parse(ChecklistRoot, filter)
    if expression is not None:
        query = query.filter(expression)

    if expand:
        query = ExpandParser.apply(query, ChecklistRoot, expand)

    total = query.count()
    roots = query.offset(skip).limit(top).all()

    response = {
        "d": {
            "results": [_project_item(item) for item in roots]
        }
    }
    if inlinecount == "allpages":
        response["d"]["__count"] = str(total)
    return response
