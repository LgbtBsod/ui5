import uuid
from datetime import datetime
from pathlib import Path

from fastapi import APIRouter, Depends, Header, Query, Request, Response
from sqlalchemy import asc, desc
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot, DictionaryItem, LockEntry
from services.hierarchy_service import HierarchyService
from services.lock_service import LockService
from services.metadata_builder import build_metadata
from utils.filter_parser import FilterParser
from utils.odata import SERVICE_ROOT, format_datetime, format_entity_etag, odata_error_response, odata_payload
from utils.time import now_utc

router = APIRouter(tags=["GatewayCanonical"])

_ATTACHMENTS: dict[str, dict] = {}
_UPLOAD_DIR = Path(__file__).resolve().parents[1] / "uploads"
_UPLOAD_DIR.mkdir(parents=True, exist_ok=True)


ROOT_MAP = {
    "Key": "id",
    "RequestId": "checklist_id",
    "Id": "checklist_id",
    "Status": "status",
    "ChangedOn": "changed_on",
    "CreatedOn": "created_on",
    "Lpc": "lpc",
}

SEARCH_MAP = {
    "Key": "id",
    "Id": "checklist_id",
    "DateCheck": "changed_on",
    "Lpc": "lpc",
    "Profession": "observed_position",
    "Status": "status",
    "ChangedOn": "changed_on",
    "EquipName": "equipment",
}

CHECK_MAP = {"RootKey": "root_id", "ChecksNum": "position", "ChangedOn": "changed_on"}
BARRIER_MAP = {"RootKey": "root_id", "BarriersNum": "position", "ChangedOn": "changed_on"}




_STATUS_TRANSITIONS = {
    "01": {"02", "03"},
    "02": {"03", "01"},
    "03": set(),
    "SUCCESS": {"WARNING", "CRITICAL"},
    "WARNING": {"SUCCESS", "CRITICAL"},
    "CRITICAL": {"WARNING", "SUCCESS"},
}


def _validate_status_change(root: ChecklistRoot, new_status: str):
    old_status = str(root.status or "")
    if old_status == new_status:
        return
    allowed = _STATUS_TRANSITIONS.get(old_status)
    if allowed is not None and new_status not in allowed:
        raise ValueError("VALIDATION_ERROR")
    required = [
        root.location_key,
        root.observer_fullname,
        root.observed_fullname,
        root.lpc,
    ]
    if any(not str(v or "").strip() for v in required):
        raise ValueError("VALIDATION_ERROR")


def _err(status: int, code: str, message: str):
    return odata_error_response(status, code, message)


def _reject_expand(expand: str | None):
    if expand:
        return _err(400, "VALIDATION_ERROR", "EXPAND_NOT_ALLOWED")
    return None


def _hex(raw: str) -> str:
    return raw.replace("-", "").lower()


def _uuid_from_hex(value: str) -> str:
    h = value.replace("-", "").lower()
    if len(h) != 32:
        return value
    try:
        return str(uuid.UUID(hex=h))
    except ValueError:
        return value


def _entity_key(key_expr: str) -> str:
    cleaned = key_expr.strip().strip("'")
    if cleaned.startswith("Key="):
        cleaned = cleaned.split("=", 1)[1].strip().strip("'")
    if cleaned.startswith("RootKey="):
        cleaned = cleaned.split("=", 1)[1].strip().strip("'")
    return _uuid_from_hex(cleaned)


def _to_search(root: ChecklistRoot) -> dict:
    return {
        "Key": _hex(root.id),
        "Id": root.checklist_id or "",
        "DateCheck": format_datetime(root.changed_on),
        "Lpc": root.lpc or "",
        "Profession": root.observed_position or "",
        "EquipName": root.equipment or "",
        "Status": root.status or "",
        "ChangedOn": format_datetime(root.changed_on),
        "CreatedOn": format_datetime(root.created_on),
        "RequestId": root.checklist_id or "",
        "HasFailedChecks": any((c.status or "").upper() == "FAIL" for c in root.checks),
        "HasFailedBarriers": any(not bool(b.is_active) for b in root.barriers),
        "SuccessChecksRate": _rate(root.checks, lambda c: (c.status or "").upper() != "FAIL"),
        "SuccessBarriersRate": _rate(root.barriers, lambda b: bool(b.is_active)),
    }


def _rate(items, pred) -> float:
    if not items:
        return 1.0
    ok = sum(1 for item in items if pred(item))
    return round(ok / len(items), 4)


def _to_root(root: ChecklistRoot) -> dict:
    d = _to_search(root)
    return {
        "Key": d["Key"],
        "RequestId": d["RequestId"],
        "Id": d["Id"],
        "ChangedOn": d["ChangedOn"],
        "CreatedOn": d["CreatedOn"],
        "Status": d["Status"],
        "HasFailedChecks": d["HasFailedChecks"],
        "HasFailedBarriers": d["HasFailedBarriers"],
        "SuccessChecksRate": d["SuccessChecksRate"],
        "SuccessBarriersRate": d["SuccessBarriersRate"],
    }


def _to_basic(root: ChecklistRoot) -> dict:
    return {
        "RootKey": _hex(root.id),
        "LocationKey": root.location_key or "",
        "LocationName": root.location_name or root.location_text or "",
        "ObserverPernr": root.observer_perner or "",
        "ObserverFullname": root.observer_fullname or "",
        "ObserverPosition": root.observer_position or "",
        "ObserverOrgUnit": root.observer_orgunit or "",
        "ObservedPernr": root.observed_perner or "",
        "ObservedFullname": root.observed_fullname or "",
        "ObservedPosition": root.observed_position or "",
        "ObservedOrgUnit": root.observed_orgunit or "",
        "Lpc": root.lpc or "",
        "Profession": root.observed_position or "",
        "DateCheck": format_datetime(root.changed_on),
        "TimeCheck": "12:00:00",
        "TimeZone": "UTC",
        "EquipName": root.equipment or "",
    }


def _to_check(item: ChecklistCheck) -> dict:
    return {
        "Key": _hex(item.id),
        "RootKey": _hex(item.root_id),
        "ChecksNum": int(item.position or 0),
        "Comment": item.text or "",
        "Result": (item.status or "").upper() != "FAIL",
        "ChangedOn": format_datetime(item.changed_on),
    }


def _to_barrier(item: ChecklistBarrier) -> dict:
    return {
        "Key": _hex(item.id),
        "RootKey": _hex(item.root_id),
        "BarriersNum": int(item.position or 0),
        "Comment": item.description or "",
        "Result": bool(item.is_active),
        "ChangedOn": format_datetime(item.changed_on),
    }


def _agg_changed_on(root: ChecklistRoot):
    points = [root.changed_on, root.created_on]
    points.extend([c.changed_on for c in (root.checks or []) if c.changed_on])
    points.extend([b.changed_on for b in (root.barriers or []) if b.changed_on])
    for meta in _ATTACHMENTS.values():
        if meta.get("root_uuid") == root.id and meta.get("changed_on"):
            points.append(meta["changed_on"])
    points = [p for p in points if p is not None]
    if not points:
        return now_utc()
    return max(points)


def _is_valid_attachment_category(db: Session, category_key: str) -> bool:
    if not category_key:
        return False
    exists = db.query(DictionaryItem).filter(
        DictionaryItem.domain == "ATF_CAT",
        DictionaryItem.key == category_key,
    ).first()
    return bool(exists)


def _apply_order_filter(query, model, fmap, filter_expr, orderby, top, skip):
    expr = FilterParser.parse(model, filter_expr, field_map=fmap)
    if expr is not None:
        query = query.filter(expr)
    if orderby:
        for clause in [x.strip() for x in orderby.split(",") if x.strip()]:
            field, _, direction = clause.partition(" ")
            col = getattr(model, fmap.get(field, field), None)
            if col is not None:
                query = query.order_by(desc(col) if direction.lower() == "desc" else asc(col))
    total = query.count()
    return query.offset(skip).limit(top).all(), total


@router.get(f"{SERVICE_ROOT}/")
def service_document():
    return {"d": {"EntitySets": [
        "ChecklistSearchSet", "ChecklistRootSet", "ChecklistBasicInfoSet", "ChecklistCheckSet", "ChecklistBarrierSet",
        "DictionaryItemSet", "LastChangeSet", "LockStatusSet", "AttachmentFolderSet", "AttachmentSet"
    ]}}


@router.get(f"{SERVICE_ROOT}/$metadata")
def metadata():
    return Response(content=build_metadata(), media_type="application/xml")


@router.get(f"{SERVICE_ROOT}/ChecklistSearchSet")
@router.get("/ChecklistRoots")
@router.get("/SearchRows")
def checklist_search_set(
    filter: str | None = Query(None, alias="$filter"),
    expand: str | None = Query(None, alias="$expand"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    if (err := _reject_expand(expand)):
        return err
    rows, total = _apply_order_filter(db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)), ChecklistRoot, SEARCH_MAP, filter, orderby, top, skip)
    return odata_payload([_to_search(r) for r in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistRootSet('{{entity_key}}')")
def checklist_root_entity(entity_key: str, response: Response, expand: str | None = Query(None, alias="$expand"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(entity_key), ChecklistRoot.is_deleted.isnot(True)).first()
    if not root:
        return _err(404, "NOT_FOUND", "Checklist root not found")
    response.headers["ETag"] = format_entity_etag(_agg_changed_on(root))
    return {"d": _to_root(root)}


@router.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet('{{entity_key}}')")
def checklist_basic_info_entity(entity_key: str, expand: str | None = Query(None, alias="$expand"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(entity_key), ChecklistRoot.is_deleted.isnot(True)).first()
    if not root:
        return _err(404, "NOT_FOUND", "Checklist basic info not found")
    return {"d": _to_basic(root)}


@router.get(f"{SERVICE_ROOT}/ChecklistCheckSet")
def checklist_check_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), top: int = Query(20, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    rows, total = _apply_order_filter(db.query(ChecklistCheck), ChecklistCheck, CHECK_MAP, filter, None, top, skip)
    return odata_payload([_to_check(c) for c in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistBarrierSet")
def checklist_barrier_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), top: int = Query(20, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    rows, total = _apply_order_filter(db.query(ChecklistBarrier), ChecklistBarrier, BARRIER_MAP, filter, None, top, skip)
    return odata_payload([_to_barrier(c) for c in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/DictionaryItemSet")
def dictionary_items(filter: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    query = db.query(DictionaryItem)
    expr = FilterParser.parse(DictionaryItem, filter, field_map={"Domain": "domain", "Key": "key", "Text": "text"})
    if expr is not None:
        query = query.filter(expr)
    return odata_payload([{"Domain": x.domain, "Key": x.key, "Text": x.text} for x in query.all()])


@router.get(f"{SERVICE_ROOT}/LastChangeSet('{{entity_key}}')")
def last_change_entity(entity_key: str, response: Response, db: Session = Depends(get_db)):
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(entity_key), ChecklistRoot.is_deleted.isnot(True)).first()
    if not root:
        return _err(404, "NOT_FOUND", "Checklist not found")
    agg = _agg_changed_on(root)
    response.headers["ETag"] = format_entity_etag(agg)
    return {"d": {"RootKey": _hex(root.id), "AggChangedOn": format_datetime(agg)}}


@router.get(f"{SERVICE_ROOT}/LockStatusSet('{{entity_key}}')")
def lock_status_entity(entity_key: str, session_guid: str = Query("", alias="SessionGuid"), uname: str = Query("", alias="Uname"), db: Session = Depends(get_db)):
    root_uuid = _entity_key(entity_key)
    active = LockService._active_lock(db, root_uuid)
    if not active:
        if session_guid:
            own = db.query(LockEntry).filter(LockEntry.pcct_uuid == root_uuid, LockEntry.session_guid == session_guid).order_by(LockEntry.last_heartbeat.desc()).first()
            if own:
                if own.is_killed:
                    return {"d": {"RootKey": _hex(root_uuid), "Ok": False, "ReasonCode": "KILLED", "Owner": own.user_id or "", "ExpiresOn": format_datetime(own.expires_at)}}
                if own.expires_at and own.expires_at < now_utc():
                    return {"d": {"RootKey": _hex(root_uuid), "Ok": False, "ReasonCode": "EXPIRED", "Owner": own.user_id or "", "ExpiresOn": format_datetime(own.expires_at)}}
        return {"d": {"RootKey": _hex(root_uuid), "Ok": True, "ReasonCode": "FREE", "Owner": "", "ExpiresOn": None}}
    if active.session_guid == session_guid and session_guid:
        return {"d": {"RootKey": _hex(root_uuid), "Ok": True, "ReasonCode": "OWNED_BY_YOU", "Owner": active.user_id, "ExpiresOn": format_datetime(active.expires_at)}}
    reason = "LOCKED_BY_OTHER"
    if active.is_killed:
        reason = "KILLED"
    return {"d": {"RootKey": _hex(root_uuid), "Ok": False, "ReasonCode": reason, "Owner": active.user_id, "ExpiresOn": format_datetime(active.expires_at)}}


@router.post(f"{SERVICE_ROOT}/LockControl")
@router.post("/actions/LockControl")
def lock_control(payload: dict, db: Session = Depends(get_db)):
    action = str(payload.get("Action") or "").upper()
    root_key = payload.get("RootKey")
    if not root_key:
        return _err(400, "VALIDATION_ERROR", "RootKey is required")
    root_uuid = _entity_key(str(root_key))
    session = str(payload.get("SessionGuid") or "")
    uname = str(payload.get("Uname") or "ANON")
    if action == "ACQUIRE":
        result = LockService.acquire(db, root_uuid, session, uname, payload.get("StealFrom"))
        return {"d": {"Ok": bool(result.get("success")), "ReasonCode": None if result.get("success") else "LOCKED_BY_OTHER", "ExpiresOn": format_datetime(result.get("lock_expires")), "IsKilled": bool(result.get("is_killed_flag"))}}
    if action == "HEARTBEAT":
        try:
            result = LockService.heartbeat(db, root_uuid, session)
            return {"d": {"Ok": bool(result.get("success")), "ReasonCode": "OWNED_BY_YOU" if result.get("success") else "KILLED", "ExpiresOn": format_datetime(result.get("lock_expires")), "IsKilled": bool(result.get("is_killed"))}}
        except ValueError:
            return _err(410, "LOCK_EXPIRED", "Lock expired")
    if action == "RELEASE":
        result = LockService.release(db, root_uuid, session)
        return {"d": {"Ok": bool(result.get("released")), "ReasonCode": "FREE", "ExpiresOn": None, "IsKilled": False}}
    return _err(400, "VALIDATION_ERROR", "Unsupported Action")


@router.post("/actions/LockAcquire")
@router.post("/actions/LockHeartbeat")
@router.post("/actions/LockRelease")
async def lock_actions_alias(request: Request, db: Session = Depends(get_db)):
    raw_payload = await request.json() if request.headers.get("content-type", "").startswith("application/json") else {}
    payload = _payload_with_query(raw_payload, request)
    path = request.url.path
    action = "ACQUIRE" if path.endswith("LockAcquire") else "HEARTBEAT" if path.endswith("LockHeartbeat") else "RELEASE"
    adapted = {
        "Action": action,
        "RootKey": payload.get("root_id") or payload.get("object_uuid") or payload.get("RootKey"),
        "SessionGuid": payload.get("session_guid") or payload.get("SessionGuid"),
        "Uname": payload.get("uname") or payload.get("user_id") or payload.get("Uname") or "ANON",
        "StealFrom": payload.get("iv_steal_from") or payload.get("StealFrom"),
    }
    return lock_control(adapted, db)


@router.post("/actions/LockStatus")
@router.post("/lock/status")
async def lock_status_alias(request: Request, db: Session = Depends(get_db)):
    raw_payload = await request.json() if request.headers.get("content-type", "").startswith("application/json") else {}
    payload = _payload_with_query(raw_payload, request)
    return lock_status_entity(
        entity_key=str(payload.get("root_id") or payload.get("object_uuid") or payload.get("RootKey") or ""),
        session_guid=str(payload.get("session_guid") or payload.get("SessionGuid") or ""),
        uname=str(payload.get("uname") or payload.get("user_id") or payload.get("Uname") or ""),
        db=db
    )


@router.post("/lock/acquire")
@router.post("/lock/heartbeat")
@router.post("/lock/release")
async def legacy_lock_alias(request: Request, db: Session = Depends(get_db)):
    return await lock_actions_alias(request, db)


@router.post("/actions/AutoSave")
@router.post("/actions/AutoSaveChecklist")
async def autosave_alias(request: Request, db: Session = Depends(get_db)):
    raw_payload = await request.json()
    payload = _payload_with_query(raw_payload, request)
    body = payload.get("payload") or payload.get("Payload") or {}
    adapted = {
        "RootKey": body.get("Uuid") or payload.get("root_id"),
        "ClientAggChangedOn": payload.get("ClientAggChangedOn") or body.get("ClientAggChangedOn"),
        "Changes": payload.get("Changes") or body.get("Changes") or []
    }
    return auto_save(adapted, None, db)


@router.post("/actions/SaveChanges")
@router.post("/actions/SaveChecklist")
async def save_alias(request: Request, db: Session = Depends(get_db)):
    raw_payload = await request.json()
    payload = _payload_with_query(raw_payload, request)
    body = payload.get("payload") or payload.get("Payload") or {}
    adapted = {
        "RootKey": body.get("Uuid") or payload.get("root_id"),
        "ClientAggChangedOn": payload.get("ClientAggChangedOn") or body.get("ClientAggChangedOn"),
        "FullPayload": payload.get("FullPayload") or body.get("FullPayload") or {"root": body, "basic": {}, "checks": [], "barriers": []}
    }
    return save_changes(adapted, None, db)


@router.post("/actions/GetMplHierarchy")
async def hierarchy_alias(request: Request, db: Session = Depends(get_db)):
    raw_payload = await request.json() if request.headers.get("content-type", "").startswith("application/json") else {}
    payload = _payload_with_query(raw_payload, request)
    return get_hierarchy(
        date_check=payload.get("date") or payload.get("DateCheck"),
        method=payload.get("method") or payload.get("Method") or "MPL",
        payload=None,
        db=db,
    )


@router.post("/actions/export")
async def export_alias(request: Request, db: Session = Depends(get_db)):
    raw_payload = await request.json() if request.headers.get("content-type", "").startswith("application/json") else {}
    payload = _payload_with_query(raw_payload, request)
    adapted = {"RootKeys": payload.get("RootKeys") or payload.get("ids") or payload.get("keys") or []}
    return report_export(adapted, db)


def _optimistic_check(payload: dict, root: ChecklistRoot):
    client = payload.get("ClientAggChangedOn")
    if not client:
        return
    try:
        cms = int(str(client).replace("/Date(", "").replace(")/", ""))
    except ValueError:
        return
    current = int(_agg_changed_on(root).timestamp() * 1000)
    if cms != current:
        raise ValueError("CONFLICT")


def _payload_with_query(payload: dict, request: Request) -> dict:
    merged = dict(payload or {})
    for key, value in request.query_params.items():
        if key not in merged:
            merged[key] = value
    return merged


def _require_client_agg(payload: dict):
    if not payload.get("ClientAggChangedOn"):
        raise ValueError("VALIDATION_ERROR")


def _if_match_check(if_match: str | None, agg: datetime):
    if not if_match or if_match == "*":
        return
    expected = format_entity_etag(agg)
    if if_match != expected:
        raise ValueError("PRECONDITION_FAILED")


@router.post(f"{SERVICE_ROOT}/AutoSave")
def auto_save(payload: dict, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(str(payload.get("RootKey") or "")), ChecklistRoot.is_deleted.isnot(True)).first()
    if not root:
        return _err(404, "NOT_FOUND", "Checklist not found")
    agg_before = _agg_changed_on(root)
    try:
        _if_match_check(if_match, agg_before)
    except ValueError:
        return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
    try:
        _require_client_agg(payload)
    except ValueError:
        return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
    try:
        _optimistic_check(payload, root)
    except ValueError:
        return _err(409, "CONFLICT", "AggChangedOn mismatch")
    for change in payload.get("Changes") or []:
        entity = str(change.get("Entity") or "").upper()
        mode = str(change.get("EditMode") or "U").upper()
        fields = change.get("Fields") or {}
        key = _entity_key(str(change.get("Key") or ""))
        if entity == "ROOT" and mode == "U":
            for k, v in fields.items():
                attr = ROOT_MAP.get(k) or ROOT_MAP.get(k[:1].upper() + k[1:])
                if attr and hasattr(root, attr):
                    setattr(root, attr, v)
        if entity == "BASIC" and mode == "U":
            root.location_key = fields.get("LocationKey", root.location_key)
            root.location_name = fields.get("LocationName", root.location_name)
            root.equipment = fields.get("EquipName", root.equipment)
        if entity == "CHECK":
            if mode == "C":
                db.add(ChecklistCheck(id=str(uuid.uuid4()), root_id=root.id, text=fields.get("Comment", ""), status="PASS" if fields.get("Result", True) else "FAIL", position=int(fields.get("ChecksNum", 0))))
            elif mode == "U":
                row = db.query(ChecklistCheck).filter(ChecklistCheck.id == key).first()
                if row:
                    if "Comment" in fields:
                        row.text = fields["Comment"]
                    if "Result" in fields:
                        row.status = "PASS" if fields["Result"] else "FAIL"
            elif mode == "D":
                db.query(ChecklistCheck).filter(ChecklistCheck.id == key).delete()
        if entity == "BARRIER":
            if mode == "C":
                db.add(ChecklistBarrier(id=str(uuid.uuid4()), root_id=root.id, description=fields.get("Comment", ""), is_active=bool(fields.get("Result", True)), position=int(fields.get("BarriersNum", 0))))
            elif mode == "U":
                row = db.query(ChecklistBarrier).filter(ChecklistBarrier.id == key).first()
                if row:
                    if "Comment" in fields:
                        row.description = fields["Comment"]
                    if "Result" in fields:
                        row.is_active = bool(fields["Result"])
            elif mode == "D":
                db.query(ChecklistBarrier).filter(ChecklistBarrier.id == key).delete()
    root.changed_on = now_utc()
    db.commit()
    agg = _agg_changed_on(root)
    return {"d": {"RootKey": _hex(root.id), "AggChangedOn": format_datetime(agg), "UpdatedKeysMapping": []}}


@router.post(f"{SERVICE_ROOT}/SaveChanges")
def save_changes(payload: dict, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(str(payload.get("RootKey") or "")), ChecklistRoot.is_deleted.isnot(True)).first()
    if not root:
        return _err(404, "NOT_FOUND", "Checklist not found")
    agg_before = _agg_changed_on(root)
    try:
        _if_match_check(if_match, agg_before)
    except ValueError:
        return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
    try:
        _require_client_agg(payload)
    except ValueError:
        return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
    try:
        _optimistic_check(payload, root)
    except ValueError:
        return _err(409, "CONFLICT", "AggChangedOn mismatch")
    full = payload.get("FullPayload") or {}
    for k, v in (full.get("root") or {}).items():
        attr = ROOT_MAP.get(k)
        if attr and hasattr(root, attr):
            setattr(root, attr, v)
    basic = full.get("basic") or {}
    root.location_key = basic.get("LocationKey", root.location_key)
    root.location_name = basic.get("LocationName", root.location_name)
    root.equipment = basic.get("EquipName", root.equipment)
    db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root.id).delete()
    db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root.id).delete()
    for c in full.get("checks") or []:
        db.add(ChecklistCheck(id=str(uuid.uuid4()), root_id=root.id, text=c.get("Comment", ""), status="PASS" if c.get("Result", True) else "FAIL", position=int(c.get("ChecksNum", 0))))
    for b in full.get("barriers") or []:
        db.add(ChecklistBarrier(id=str(uuid.uuid4()), root_id=root.id, description=b.get("Comment", ""), is_active=bool(b.get("Result", True)), position=int(b.get("BarriersNum", 0))))
    root.changed_on = now_utc()
    db.commit()
    db.refresh(root)
    d = _to_root(root)
    d["RootKey"] = d.pop("Key")
    d["AggChangedOn"] = format_datetime(_agg_changed_on(root))
    return {"d": d}


@router.post("/actions/SetChecklistStatus")
@router.post(f"{SERVICE_ROOT}/SetChecklistStatus")
def set_status(payload: dict, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(str(payload.get("RootKey") or "")), ChecklistRoot.is_deleted.isnot(True)).first()
    if not root:
        return _err(404, "NOT_FOUND", "Checklist not found")
    agg_before = _agg_changed_on(root)
    try:
        _if_match_check(if_match, agg_before)
    except ValueError:
        return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
    try:
        _require_client_agg(payload)
    except ValueError:
        return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
    try:
        _optimistic_check(payload, root)
    except ValueError:
        return _err(409, "CONFLICT", "AggChangedOn mismatch")
    new_status = str(payload.get("NewStatus") or "")
    if new_status not in {"01", "02", "03", "SUCCESS", "WARNING", "CRITICAL"}:
        return _err(400, "VALIDATION_ERROR", "Unsupported status")
    try:
        _validate_status_change(root, new_status)
    except ValueError:
        return _err(400, "VALIDATION_ERROR", "Invalid status transition or required fields missing")
    root.status = new_status
    root.changed_on = now_utc()
    db.commit()
    return {"d": {"RootKey": _hex(root.id), "Status": root.status, "AggChangedOn": format_datetime(_agg_changed_on(root))}}


@router.api_route(f"{SERVICE_ROOT}/GetHierarchy", methods=["GET", "POST"])
def get_hierarchy(date_check: str | None = Query(None, alias="DateCheck"), method: str | None = Query("LOCATION", alias="Method"), payload: dict | None = None, db: Session = Depends(get_db)):
    if payload:
        date_check = payload.get("DateCheck") or date_check
        method = payload.get("Method") or method
    if not date_check:
        date_check = now_utc().date().isoformat()
    dt = datetime.fromisoformat(str(date_check).replace("Z", "+00:00")).date()
    rows = HierarchyService.get_tree(db, dt)
    return {"d": {"results": [{
        "hierarchy_rank": r.get("hierarchy_rank", 0),
        "hierarchy_tree_size": r.get("hierarchy_tree_size", 1),
        "hierarchy_parent_rank": r.get("hierarchy_parent_rank", 0),
        "hierarchy_level": r.get("hierarchy_level", 0),
        "node_id": r.get("node_id"),
        "parent_id": r.get("parent_id"),
        "location_name": r.get("location_name"),
        "drill_state": r.get("drill_state", "leaf"),
        "method": method,
    } for r in rows]}}


@router.post(f"{SERVICE_ROOT}/ReportExport")
def report_export(payload: dict, db: Session = Depends(get_db)):
    ordered_keys = payload.get("RootKeys") or []
    rows = []
    for key in ordered_keys:
        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(str(key)), ChecklistRoot.is_deleted.isnot(True)).first()
        if not root:
            continue
        for c in root.checks:
            rows.append({"RootKey": _hex(root.id), "Id": root.checklist_id, "Lpc": root.lpc, "Profession": root.observed_position or "", "DateCheck": format_datetime(root.changed_on), "Type": "CHECK", "Num": c.position or 0, "Text": c.text or "", "Comment": c.text or "", "Result": (c.status or "").upper() != "FAIL"})
        for b in root.barriers:
            rows.append({"RootKey": _hex(root.id), "Id": root.checklist_id, "Lpc": root.lpc, "Profession": root.observed_position or "", "DateCheck": format_datetime(root.changed_on), "Type": "BARRIER", "Num": b.position or 0, "Text": b.description or "", "Comment": b.description or "", "Result": bool(b.is_active)})
    return {"d": {"results": rows}}


@router.get(f"{SERVICE_ROOT}/AttachmentFolderSet")
def attachment_folders(filter: str | None = Query(None, alias="$filter")):
    root_key = None
    if filter and "RootKey" in filter:
        root_key = filter.split("'")[1]
    roots = {meta["root_key"] for meta in _ATTACHMENTS.values() if not root_key or meta["root_key"] == root_key}
    return odata_payload([{"FolderKey": f"F-{rk[:8]}", "RootKey": rk, "Title": "Default", "CreatedOn": format_datetime(now_utc()), "ChangedOn": format_datetime(now_utc())} for rk in roots])


@router.get(f"{SERVICE_ROOT}/AttachmentSet")
def attachment_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand")):
    if (err := _reject_expand(expand)):
        return err
    items = list(_ATTACHMENTS.values())
    if filter and "RootKey" in filter:
        key = filter.split("'")[1]
        items = [x for x in items if x["root_key"] == key]
    return odata_payload([{
        "AttachmentKey": x["attachment_key"], "RootKey": x["root_key"], "FolderKey": x["folder_key"], "CategoryKey": x.get("category_key", "GEN"),
        "FileName": x["filename"], "MimeType": x["mime"], "FileSize": x["size"], "ScanStatus": "OK", "ScannedOn": format_datetime(x.get("scanned_on") or x["changed_on"]),
        "CreatedOn": format_datetime(x["created_on"]), "ChangedOn": format_datetime(x["changed_on"])
    } for x in items])


@router.put(f"{SERVICE_ROOT}/AttachmentSet('{{entity_key}}')/$value")
async def attachment_upload(entity_key: str, request: Request, content_type: str | None = Header(None), db: Session = Depends(get_db)):
    data = await request.body()
    attachment_key = entity_key.strip("'")
    root_key = request.query_params.get("RootKey") or ""
    root_uuid = _entity_key(root_key)
    if not root_uuid:
        return _err(400, "VALIDATION_ERROR", "RootKey is required")
    file_path = _UPLOAD_DIR / f"{attachment_key}.bin"
    file_path.write_bytes(data)
    now = now_utc()
    category_key = request.query_params.get("CategoryKey") or "GEN"
    if not _is_valid_attachment_category(db, category_key):
        return _err(400, "VALIDATION_ERROR", "Unsupported attachment category")
    _ATTACHMENTS[attachment_key] = {
        "attachment_key": attachment_key,
        "root_key": _hex(root_uuid),
        "root_uuid": root_uuid,
        "folder_key": f"F-{_hex(root_uuid)[:8]}",
        "filename": request.query_params.get("FileName") or f"{attachment_key}.bin",
        "mime": content_type or "application/octet-stream",
        "size": len(data),
        "category_key": category_key,
        "created_on": now,
        "changed_on": now,
        "scanned_on": now,
        "path": str(file_path),
    }
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_uuid).first()
    if root:
        root.changed_on = now
        db.commit()
    return Response(status_code=204)


@router.get(f"{SERVICE_ROOT}/AttachmentSet('{{entity_key}}')/$value")
def attachment_get_value(entity_key: str):
    meta = _ATTACHMENTS.get(entity_key.strip("'"))
    if not meta:
        return _err(404, "NOT_FOUND", "Attachment not found")
    return Response(content=Path(meta["path"]).read_bytes(), media_type=meta["mime"])


@router.delete(f"{SERVICE_ROOT}/AttachmentSet('{{entity_key}}')")
def attachment_delete(entity_key: str, db: Session = Depends(get_db)):
    key = entity_key.strip("'")
    meta = _ATTACHMENTS.pop(key, None)
    if not meta:
        return _err(404, "NOT_FOUND", "Attachment not found")
    path = Path(meta["path"])
    if path.exists():
        path.unlink()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == meta["root_uuid"]).first()
    if root:
        root.changed_on = now_utc()
        db.commit()
    return Response(status_code=204)
