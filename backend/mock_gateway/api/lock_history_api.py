from fastapi import APIRouter, Depends, Query
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import LockLog
from utils.filter_parser import FilterParser

router = APIRouter(tags=["LockHistory"])


def _apply_orderby(query, orderby: str | None):
    if not orderby:
        return query.order_by(LockLog.timestamp.desc())

    clauses = []
    allowed = {
        "timestamp": LockLog.timestamp,
        "action": LockLog.action,
        "pcct_uuid": LockLog.pcct_uuid,
        "user_id": LockLog.user_id,
        "session_guid": LockLog.session_guid,
    }
    for token in [t.strip() for t in orderby.split(",") if t.strip()]:
        parts = token.split()
        name = parts[0]
        col = allowed.get(name)
        if not col:
            continue
        is_desc = len(parts) > 1 and parts[1].lower() == "desc"
        clauses.append(col.desc() if is_desc else col.asc())

    if clauses:
        return query.order_by(*clauses)
    return query.order_by(LockLog.timestamp.desc())


@router.get("/LockLogs")
def lock_logs(
    filter: str | None = Query(default=None, alias="$filter"),
    orderby: str | None = Query(default="timestamp desc", alias="$orderby"),
    top: int = Query(default=DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(default=0, alias="$skip"),
    inlinecount: str | None = Query(default=None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    query = db.query(LockLog)

    expression = FilterParser.parse(LockLog, filter)
    if expression is not None:
        query = query.filter(expression)

    total = query.count()
    rows = _apply_orderby(query, orderby).offset(skip).limit(top).all()

    payload = {
        "d": {
            "results": [
                {
                    "id": row.id,
                    "pcct_uuid": row.pcct_uuid,
                    "user_id": row.user_id,
                    "session_guid": row.session_guid,
                    "action": row.action,
                    "timestamp": row.timestamp,
                }
                for row in rows
            ]
        }
    }
    if inlinecount == "allpages":
        payload["d"]["__count"] = str(total)
    return payload
