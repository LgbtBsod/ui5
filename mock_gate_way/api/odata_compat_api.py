import logging
from fastapi import APIRouter, Depends, Query
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistRoot
from utils.filter_parser import FilterParser

router = APIRouter(tags=["ODataCompatibility"])
logger = logging.getLogger("gateway.odata")


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
    select: str | None = Query(default=None, alias="$select"),
    db: Session = Depends(get_db)
):
    logger.info(
        "ChecklistRoots query filter=%r expand=%r select=%r top=%s skip=%s inlinecount=%r",
        filter,
        expand,
        select,
        top,
        skip,
        inlinecount,
    )

    query = db.query(ChecklistRoot).filter((ChecklistRoot.is_deleted.is_(None)) | (ChecklistRoot.is_deleted.is_(False)))

    expression = FilterParser.parse(ChecklistRoot, filter)
    if expression is not None:
        logger.info("ChecklistRoots parsed filter expression=%s", expression)
        query = query.filter(expression)

    if expand:
        logger.warning("ChecklistRoots ignores $expand=%s by design (lazy object-flow policy)", expand)

    total = query.count()
    roots = query.offset(skip).limit(top).all()
    logger.info("ChecklistRoots result total=%s page_rows=%s", total, len(roots))

    response = {
        "d": {
            "results": [_project_item(item) for item in roots]
        }
    }
    if inlinecount == "allpages":
        response["d"]["__count"] = str(total)
    return response
