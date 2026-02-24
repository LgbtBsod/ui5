from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from database import get_db
from services.checklist_service import ChecklistService
from sqlalchemy import func, or_, and_
from utils.odata_filter import ODataFilterParser
from utils.expand_parser import ExpandParser

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

@router.get("")
def suggest(
    search: str = None,
    checklist_id: str = None,
    db: Session = Depends(get_db)
):
    query = db.query(Person)

    # 1️⃣ Фильтр по дате
    if checklist_id:
        root = db.query(ChecklistRoot)\
            .filter(ChecklistRoot.id == checklist_id)\
            .first()

        if root:
            check_date = root.date_check

            query = query.filter(
                Person.begda <= check_date,
                (Person.endda == None) | (Person.endda >= check_date)
            )

    # 2️⃣ Фильтр по ФИО (конкатенация)
    if search:
        full_name_expr = func.trim(
            Person.last_name + " " +
            Person.first_name + " " +
            Person.middle_name
        )

        pattern = f"%{search.lower()}%"

        query = query.filter(
            func.lower(full_name_expr).like(pattern)
        )

    results = query.limit(20).all()

    return {
        "value": [
            {
                "perner": p.perner,
                "fullName": f"{p.last_name} {p.first_name} {p.middle_name}",
                "position": p.position,
                "orgUnit": p.org_unit
            }
            for p in results
        ]
    }

    @router.get("/{root_id}")
def get(
    root_id: str,
    request: Request,
    response: Response,
    expand: str = None,
    db: Session = Depends(get_db)
):
    query = db.query(ChecklistRoot)

    query = ExpandParser.apply(query, ChecklistRoot, expand)

    root = query.filter(ChecklistRoot.id == root_id).first()

    if not root:
        raise HTTPException(status_code=404)

    etag_date = ChecklistService.calculate_etag(db, root_id)
    etag = etag_date.isoformat()

    client_etag = request.headers.get("If-None-Match")

    if client_etag == etag:
        response.status_code = 304
        return

    response.headers["ETag"] = etag

    result = {
        "id": root.id,
        "title": root.title,
        "lpc_level": root.lpc_level,
        "date_check": root.date_check,
    }

    if expand:
        if "barriers" in expand:
            result["barriers"] = [
                {
                    "id": b.id,
                    "name": b.name
                }
                for b in root.barriers
            ]

        if "checks" in expand:
            result["checks"] = [
                {
                    "id": c.id,
                    "description": c.description
                }
                for c in root.checks
            ]

    return result

    @router.get("")
def list_checklists(
    expand: str = None,
    db: Session = Depends(get_db)
):

    query = db.query(ChecklistRoot)

    query = ExpandParser.apply(query, ChecklistRoot, expand)

    roots = query.all()

    result = []

    for r in roots:

        item = {
            "id": r.id,
            "title": r.title,
            "lpc_level": r.lpc_level
        }

        if expand:
            if "barriers" in expand:
                item["barriers"] = [
                    {"id": b.id, "name": b.name}
                    for b in r.barriers
                ]

            if "checks" in expand:
                item["checks"] = [
                    {"id": c.id, "description": c.description}
                    for c in r.checks
                ]

        result.append(item)

    return {"value": result}


    @router.get("/{root_id}/barriers")
def get_barriers(
    root_id: str,
    top: int = 50,
    skip: int = 0,
    db: Session = Depends(get_db)
):

    query = db.query(Barrier).filter(
        Barrier.root_id == root_id
    )

    total = query.count()

    items = query.offset(skip).limit(top).all()

    return {
        "value": [
            {
                "id": b.id,
                "name": b.name
            }
            for b in items
        ],
        "count": total
    }


    @router.get("/{root_id}/checks")
def get_checks(
    root_id: str,
    top: int = 50,
    skip: int = 0,
    db: Session = Depends(get_db)
):

    query = db.query(Check).filter(
        Check.root_id == root_id
    )

    total = query.count()

    items = query.offset(skip).limit(top).all()

    return {
        "value": [
            {
                "id": c.id,
                "description": c.description
            }
            for c in items
        ],
        "count": total
    }

    @router.post("/{root_id}/copy")
def copy(root_id: str, db: Session = Depends(get_db)):

    new_root = ChecklistService.copy(db, root_id)

    return {
        "new_id": new_root.id
    }

    @router.post("/{root_id}/release")
def release(root_id: str, db: Session = Depends(get_db)):

    root = db.query(ChecklistRoot).filter(
        ChecklistRoot.id == root_id
    ).first()

    if not root:
        raise HTTPException(status_code=404)

    root.status = "RELEASED"
    root.changed_on = now_utc()

    db.commit()

    return {"status": "released"}

    @router.post("/{root_id}/lock")
def lock(root_id: str, user: str, db: Session = Depends(get_db)):

    success = LockService.lock(db, root_id, user)

    if not success:
        raise HTTPException(status_code=409, detail="Already locked")

    return {"status": "locked"}

    @router.post("/{root_id}/unlock")
def unlock(root_id: str, user: str, db: Session = Depends(get_db)):

    LockService.unlock(db, root_id, user)

    return {"status": "unlocked"}

    status = Column(String, default="DRAFT")

    @router.post("/{root_id}/approve")
def approve(root_id: str, user: str, db: Session = Depends(get_db)):

    root = db.query(ChecklistRoot).filter(
        ChecklistRoot.id == root_id
    ).first()

    if not root:
        raise HTTPException(status_code=404)

    if not LockService.is_locked_by_user(db, root_id, user):
        raise HTTPException(status_code=403, detail="Not locked by user")

    root.status = "APPROVED"
    root.changed_on = now_utc()

    db.commit()

    LastChangeService.touch_entity(db, "ChecklistRoot", root_id)

    return {"status": "approved"}

    @router.post("/{root_id}/import-hierarchy")
def import_hierarchy(root_id: str, db: Session = Depends(get_db)):

    root = db.query(ChecklistRoot)\
        .filter(ChecklistRoot.id == root_id)\
        .first()

    if not root:
        raise HTTPException(status_code=404)

    # вызываем сервис
    tree = LocationService.get_tree(db, root.date_check)

    return {
        "value": tree
    }

    @router.patch("/{root_id}")
def update_checklist(
    root_id: str,
    request: Request,
    payload: dict,
    db: Session = Depends(get_db)
):
    root = db.query(ChecklistRoot)\
        .filter(ChecklistRoot.id == root_id)\
        .first()

    if not root:
        raise HTTPException(status_code=404)

    # 🔥 1. Проверяем If-Match
    client_etag = request.headers.get("If-Match")

    if not client_etag:
        raise HTTPException(
            status_code=428,
            detail="ETag required (If-Match header missing)"
        )

    current_etag_date = ChecklistService.calculate_etag(db, root_id)

    if not current_etag_date:
        raise HTTPException(status_code=409)

    current_etag = current_etag_date.isoformat()

    if client_etag != current_etag:
        raise HTTPException(
            status_code=412,
            detail="ETag mismatch — object was modified"
        )

    # 🔥 2. Обновляем данные
    for key, value in payload.items():
        if hasattr(root, key):
            setattr(root, key, value)

    root.changed_on = now_utc()

    db.commit()

    return {"status": "updated"}