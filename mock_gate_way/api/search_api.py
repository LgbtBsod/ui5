from fastapi import APIRouter, Depends, Query
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistRoot
from utils.filter_parser import FilterParser

router = APIRouter(tags=["SearchRows"])


def _map_row(item: ChecklistRoot):
    return {
        "db_key": item.id,
        "checklist_id": item.checklist_id,
        "lpc": item.lpc,
        "status": item.status,
        "date": item.date,
        "equipment": item.equipment,
        "changed_on": item.changed_on,
        "changed_by": item.changed_by,
    }


@router.get("/SearchRows")
def search_rows(
    filter: str | None = Query(default=None, alias="$filter"),
    top: int = Query(default=DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(default=0, alias="$skip"),
    inlinecount: str | None = Query(default=None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True))
    expression = FilterParser.parse(ChecklistRoot, filter)
    if expression is not None:
        query = query.filter(expression)

    total = query.count()
    rows = query.offset(skip).limit(top).all()

    payload = {"d": {"results": [_map_row(x) for x in rows]}}
    if inlinecount == "allpages":
        payload["d"]["__count"] = str(total)
    return payload
