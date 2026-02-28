import json
import logging
import os
import shutil
import tempfile
import uuid
from contextlib import contextmanager
from datetime import date

from fastapi import APIRouter, Depends, Header, Query, Request, Response
from sqlalchemy import asc, desc
from sqlalchemy.orm import Session

from config import DATABASE_URL, DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot, LastChangeSet, LockEntry, Person
from services.checklist_service import ChecklistService
from services.hierarchy_service import HierarchyService
from services.lock_service import LockService
from services.metadata_builder import build_metadata
from utils.filter_parser import FilterParser
from utils.odata import SERVICE_ROOT, format_datetime, format_entity_etag, odata_error_response, odata_payload
from utils.odata_mapping import (
    BARRIER_MAP,
    CHECKLIST_FLAT_MAP,
    CHECKLIST_ROOT_MAP,
    CHECK_MAP,
    LAST_CHANGE_MAP,
    PERSON_VH_MAP,
    barrier_to_odata,
    check_to_odata,
    flat_to_odata,
    last_change_to_odata,
    parse_select,
    person_to_odata,
    root_to_odata,
)
from utils.time import now_utc

router = APIRouter(tags=["ODataCompatibility"])
logger = logging.getLogger("gateway.odata")


ERROR_HTTP_MAP = {
    "LOCKED": 409,
    "ALREADY_OPEN_BY_YOU": 409,
    "STEAL_KILLED": 409,
    "CONFLICT": 409,
    "LOCK_EXPIRED": 410,
    "VALIDATION_ERROR": 400,
    "AUTH_ERROR": 403,
    "NOT_FOUND": 404,
    "SYSTEM_ERROR": 500,
}


@contextmanager
def _sqlite_backup_guard():
    if not DATABASE_URL.startswith("sqlite:///"):
        yield
        return
    db_path = DATABASE_URL.replace("sqlite:///", "", 1)
    if not os.path.exists(db_path):
        yield
        return
    fd, backup_path = tempfile.mkstemp(prefix="changeset_", suffix=".db")
    os.close(fd)
    shutil.copyfile(db_path, backup_path)
    try:
        yield
    except Exception:
        shutil.copyfile(backup_path, db_path)
        raise
    finally:
        if os.path.exists(backup_path):
            os.remove(backup_path)


def _error(code: str, message: str, details: list[dict] | None = None):
    return odata_error_response(ERROR_HTTP_MAP.get(code, 500), code, message, details)


def _reject_expand(expand: str | None):
    if expand:
        return _error("VALIDATION_ERROR", "EXPAND_NOT_ALLOWED")
    return None


def _rows(query, model, field_map, filter_expr, orderby, top, skip):
    expression = FilterParser.parse(model, filter_expr, field_map=field_map)
    if expression is not None:
        query = query.filter(expression)
    if orderby:
        for clause in [x.strip() for x in orderby.split(",") if x.strip()]:
            field, _, direction = clause.partition(" ")
            attr = field_map.get(field, field)
            col = getattr(model, attr, None)
            if col is not None:
                query = query.order_by(desc(col) if direction.lower() == "desc" else asc(col))
    total = query.count()
    return query.offset(skip).limit(top).all(), total


def _etag(root: ChecklistRoot) -> str | None:
    return format_entity_etag(root.version_number, root.changed_on)


def _increment_version(root: ChecklistRoot):
    root.version_number = int(root.version_number or 0) + 1
    root.changed_on = now_utc()


def _extract_payload_value(payload: dict, key: str, fallback: str | None = None):
    return payload.get(key) or payload.get(key.lower()) or (payload.get(fallback) if fallback else None)


@router.get(f"{SERVICE_ROOT}/")
def service_document():
    return {"d": {"EntitySets": ["ChecklistSet", "ChecklistFlatSet", "CheckSet", "BarrierSet", "LastChangeSet", "PersonVHSet", "AttachmentSet", "MplTreeSet"]}}


@router.get(f"{SERVICE_ROOT}/$metadata")
def metadata():
    return Response(content=build_metadata(), media_type="application/xml")


@router.get(f"{SERVICE_ROOT}/ChecklistFlatSet")
@router.get("/ChecklistRoots")
@router.get("/SearchRows")
def checklist_flat(
    filter: str | None = Query(None, alias="$filter"),
    expand: str | None = Query(None, alias="$expand"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    select: str | None = Query(None, alias="$select"),
    db: Session = Depends(get_db),
):
    if (err := _reject_expand(expand)):
        return err
    rows, total = _rows(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        CHECKLIST_FLAT_MAP,
        filter,
        orderby,
        top,
        skip,
    )
    selected = parse_select(select)
    return odata_payload([flat_to_odata(x, selected) for x in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistSet({{entity_key}})")
def checklist_root(entity_key: str, expand: str | None = Query(None, alias="$expand"), response: Response = None, db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    root_id = entity_key.strip("'")
    row = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_id, ChecklistRoot.is_deleted.isnot(True)).first()
    if not row:
        return _error("NOT_FOUND", "Checklist not found")
    etag = _etag(row)
    if etag and response is not None:
        response.headers["ETag"] = etag
    return {"d": root_to_odata(row)}


@router.api_route(f"{SERVICE_ROOT}/ChecklistSet({{entity_key}})", methods=["MERGE", "PATCH", "DELETE"])
async def checklist_modify(entity_key: str, request: Request, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    root_id = entity_key.strip("'")
    row = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_id, ChecklistRoot.is_deleted.isnot(True)).first()
    if not row:
        return _error("NOT_FOUND", "Checklist not found")
    if if_match is None:
        return _error("CONFLICT", "If-Match is required")
    if if_match not in {"*", _etag(row)}:
        return _error("CONFLICT", "ETag mismatch")
    if request.method == "DELETE":
        row.is_deleted = True
        _increment_version(row)
        db.commit()
        return Response(status_code=204)

    payload = await request.json()
    for k, v in payload.items():
        attr = CHECKLIST_ROOT_MAP.get(k)
        if attr and hasattr(row, attr):
            setattr(row, attr, v)
    _increment_version(row)
    db.commit()
    db.refresh(row)
    return {"d": root_to_odata(row)}


@router.get(f"{SERVICE_ROOT}/CheckSet")
def check_set(
    filter: str | None = Query(None, alias="$filter"),
    expand: str | None = Query(None, alias="$expand"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(50, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    select: str | None = Query(None, alias="$select"),
    db: Session = Depends(get_db),
):
    if (err := _reject_expand(expand)):
        return err
    rows, total = _rows(db.query(ChecklistCheck), ChecklistCheck, CHECK_MAP, filter, orderby, top, skip)
    return odata_payload([check_to_odata(x, parse_select(select)) for x in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/BarrierSet")
def barrier_set(
    filter: str | None = Query(None, alias="$filter"),
    expand: str | None = Query(None, alias="$expand"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(50, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    select: str | None = Query(None, alias="$select"),
    db: Session = Depends(get_db),
):
    if (err := _reject_expand(expand)):
        return err
    rows, total = _rows(db.query(ChecklistBarrier), ChecklistBarrier, BARRIER_MAP, filter, orderby, top, skip)
    return odata_payload([barrier_to_odata(x, parse_select(select)) for x in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/LastChangeSet")
def last_change_set(db: Session = Depends(get_db)):
    rows = db.query(LastChangeSet).order_by(LastChangeSet.last_change_timestamp.desc()).limit(200).all()
    return odata_payload([last_change_to_odata(x) for x in rows])


@router.get(f"{SERVICE_ROOT}/PersonVHSet")
def person_vh(filter: str | None = Query(None, alias="$filter"), top: int = Query(20, alias="$top"), skip: int = Query(0, alias="$skip"), db: Session = Depends(get_db)):
    rows, total = _rows(db.query(Person), Person, PERSON_VH_MAP, filter, None, top, skip)
    return odata_payload([person_to_odata(x) for x in rows], total)


@router.get(f"{SERVICE_ROOT}/AttachmentSet")
def attachment_stub():
    return odata_payload([])


@router.get(f"{SERVICE_ROOT}/MplTreeSet")
def mpl_tree_set(date_value: str = Query(..., alias="AssessDate"), db: Session = Depends(get_db)):
    tree = HierarchyService.get_tree(db, date.fromisoformat(date_value))
    results = [{"NodeId": r["node_id"], "ParentId": r["parent_id"], "Level": r["hierarchy_level"], "Text": r["location_name"], "ValidFrom": date_value, "ValidTo": ""} for r in tree]
    return odata_payload(results)


@router.post(f"{SERVICE_ROOT}/LockAcquire")
async def lock_acquire(request: Request, db: Session = Depends(get_db)):
    payload = await request.json()
    obj = _extract_payload_value(payload, "object_uuid", "ObjectUuid")
    session = _extract_payload_value(payload, "session_guid", "SessionGuid")
    user = _extract_payload_value(payload, "uname", "Uname")
    steal_from = _extract_payload_value(payload, "steal_from", "StealFrom")
    if not obj or not session or not user:
        return _error("VALIDATION_ERROR", "object_uuid, session_guid, uname are required")

    active = LockService._active_lock(db, obj)
    if active and active.user_id == user and active.session_guid != session and not steal_from:
        return _error("ALREADY_OPEN_BY_YOU", "Lock already held by your user in another session")

    result = LockService.acquire(db, obj, session, user, steal_from)
    if not result.get("success"):
        return _error("LOCKED", "Object locked by another user", [{"owner": result.get("owner", "") }])
    if result.get("is_killed_flag"):
        result["code"] = "STEAL_KILLED"
    if result.get("lock_expires"):
        result["lock_expires"] = format_datetime(result["lock_expires"])
    return {"d": result}


@router.post(f"{SERVICE_ROOT}/LockHeartbeat")
async def lock_heartbeat(request: Request, db: Session = Depends(get_db)):
    payload = await request.json()
    obj = _extract_payload_value(payload, "object_uuid", "ObjectUuid")
    session = _extract_payload_value(payload, "session_guid", "SessionGuid")
    if not obj or not session:
        return _error("VALIDATION_ERROR", "object_uuid and session_guid are required")
    try:
        result = LockService.heartbeat(db, obj, session)
    except ValueError as exc:
        code = str(exc)
        if code in {"LOCK_EXPIRED", "NO_LOCK"}:
            return _error("LOCK_EXPIRED", "Lock is expired or not found")
        return _error("SYSTEM_ERROR", code)
    if result.get("lock_expires"):
        result["lock_expires"] = format_datetime(result["lock_expires"])
    if result.get("server_changed_on"):
        result["server_changed_on"] = format_datetime(result["server_changed_on"])
    return {"d": result}


@router.post(f"{SERVICE_ROOT}/LockRelease")
async def lock_release(request: Request, db: Session = Depends(get_db)):
    payload = await request.json()
    obj = _extract_payload_value(payload, "object_uuid", "ObjectUuid")
    session = _extract_payload_value(payload, "session_guid", "SessionGuid")
    try_save = bool(_extract_payload_value(payload, "try_save", "TrySave"))
    delta = _extract_payload_value(payload, "payload", "Payload")
    if not obj or not session:
        return _error("VALIDATION_ERROR", "object_uuid and session_guid are required")
    save_status = "N"
    if try_save and delta:
        lock = db.query(LockEntry).filter(LockEntry.pcct_uuid == obj, LockEntry.session_guid == session, LockEntry.is_killed.is_(False)).first()
        if lock:
            try:
                ChecklistService.save_via_import(db, obj, lock.user_id, delta, is_autosave=True, force=True)
                save_status = "S"
            except Exception:
                save_status = "E"
    result = LockService.release(db, obj, session, try_save, delta)
    result["save_status"] = save_status
    return {"d": result}


@router.post(f"{SERVICE_ROOT}/SaveDraft")
async def save_draft(request: Request, db: Session = Depends(get_db)):
    payload = await request.json()
    data = payload.get("payload") or payload.get("Payload") or {}
    session = payload.get("session_guid") or payload.get("SessionGuid")
    user = payload.get("uname") or payload.get("Uname") or "ANON"
    temp_uuid = data.get("Uuid")

    new_root = ChecklistRoot(
        id=str(uuid.uuid4()),
        checklist_id=data.get("ChecklistId") or f"DRAFT-{uuid.uuid4().hex[:8].upper()}",
        lpc=data.get("Lpc") or "LPC-01",
        status=data.get("Status") or "01",
        changed_by=user,
        created_by=user,
        version_number=1,
    )
    db.add(new_root)
    db.commit()
    db.refresh(new_root)
    LockService.acquire(db, new_root.id, session or uuid.uuid4().hex, user, None)

    return {"d": {"real_uuid": new_root.id, "checklist_id": new_root.checklist_id, "key_mapping": [{"temp_uuid": temp_uuid or "", "real_uuid": new_root.id}], "changed_on": format_datetime(new_root.changed_on), "version_number": int(new_root.version_number or 1), "lock_expires": format_datetime(now_utc())}}


@router.post(f"{SERVICE_ROOT}/SaveChanges")
async def save_changes(request: Request, db: Session = Depends(get_db)):
    payload = await request.json()
    data = payload.get("payload") or payload.get("Payload") or {}
    session = payload.get("session_guid") or payload.get("SessionGuid")
    uuid_value = data.get("Uuid")
    client_version = int(payload.get("client_version") or payload.get("ClientVersion") or 0)
    row = db.query(ChecklistRoot).filter(ChecklistRoot.id == uuid_value, ChecklistRoot.is_deleted.isnot(True)).first()
    if not row:
        return _error("NOT_FOUND", "Checklist not found")
    if int(row.version_number or 0) != client_version:
        return _error("CONFLICT", "Version conflict")

    for k, v in data.items():
        attr = CHECKLIST_ROOT_MAP.get(k)
        if attr and hasattr(row, attr):
            setattr(row, attr, v)
    _increment_version(row)
    db.add(LastChangeSet(entity_name="Checklist", entity_id=row.id, last_change_timestamp=row.changed_on))
    db.commit()

    lock = db.query(LockEntry).filter(LockEntry.pcct_uuid == row.id, LockEntry.session_guid == session, LockEntry.is_killed.is_(False)).first() if session else None
    return {"d": {"changed_on": format_datetime(row.changed_on), "version_number": int(row.version_number), "key_mapping": [], "lock_expires": format_datetime(lock.expires_at) if lock and lock.expires_at else None}}


@router.post(f"{SERVICE_ROOT}/AutoSave")
async def autosave(request: Request, db: Session = Depends(get_db)):
    payload = await request.json()
    data = payload.get("payload") or payload.get("Payload") or {}
    uuid_value = data.get("Uuid")
    client_version = int(payload.get("client_version") or payload.get("ClientVersion") or 0)
    row = db.query(ChecklistRoot).filter(ChecklistRoot.id == uuid_value, ChecklistRoot.is_deleted.isnot(True)).first()
    if not row:
        return _error("NOT_FOUND", "Checklist not found")
    if int(row.version_number or 0) != client_version:
        return _error("CONFLICT", "Version conflict")
    for k, v in data.items():
        attr = CHECKLIST_ROOT_MAP.get(k)
        if attr and hasattr(row, attr):
            setattr(row, attr, v)
    _increment_version(row)
    db.add(LastChangeSet(entity_name="Checklist", entity_id=row.id, last_change_timestamp=row.changed_on))
    db.commit()
    return {"d": {"changed_on": format_datetime(row.changed_on), "version_number": int(row.version_number)}}


@router.post(f"{SERVICE_ROOT}/CopyChecklist")
async def copy_checklist(request: Request, db: Session = Depends(get_db)):
    payload = await request.json()
    source_uuid = payload.get("source_uuid") or payload.get("SourceUuid")
    session = payload.get("session_guid") or payload.get("SessionGuid") or uuid.uuid4().hex
    user = payload.get("uname") or payload.get("Uname") or "ANON"
    if not source_uuid:
        return _error("VALIDATION_ERROR", "source_uuid is required")
    try:
        copied = ChecklistService.copy(db, source_uuid, user)
    except ValueError:
        return _error("NOT_FOUND", "Source checklist not found")
    LockService.acquire(db, copied.id, session, user, None)
    return {"d": {"new_uuid": copied.id, "new_id": copied.checklist_id, "lock_expires": format_datetime(now_utc())}}


@router.get(f"{SERVICE_ROOT}/MplTree")
def mpl_tree_function(assess_date: str = Query(..., alias="date"), db: Session = Depends(get_db)):
    tree = HierarchyService.get_tree(db, date.fromisoformat(assess_date))
    return {"d": {"results": [{"node_id": r["node_id"], "parent_id": r["parent_id"], "level": r["hierarchy_level"], "text": r["location_name"], "valid_from": assess_date, "valid_to": ""} for r in tree]}}


@router.post(f"{SERVICE_ROOT}/$batch")
async def batch(request: Request):
    from api.batch_api import batch as raw_batch

    with _sqlite_backup_guard():
        try:
            return await raw_batch(request)
        except Exception as exc:  # noqa: BLE001
            logger.exception("Batch failed")
            return _error("SYSTEM_ERROR", f"BATCH_FAILED: {exc}")
