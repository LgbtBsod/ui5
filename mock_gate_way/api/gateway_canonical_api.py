import uuid
import re
import json
from datetime import datetime, timezone
from pathlib import Path

from fastapi import APIRouter, Depends, Header, Query, Request, Response
from sqlalchemy import asc, desc
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE
from database import get_db
from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot, DictionaryItem, FrontendRuntimeSettings, LockEntry
from services.hierarchy_service import HierarchyService
from services.lock_service import LockService
from services.metadata_builder import build_metadata
from utils.filter_parser import FilterParser
from utils.filter_engine import parse_filter_to_predicate
from utils.key_normalizer import normalize_raw16_hex
from utils.odata import SERVICE_ROOT, format_datetime, format_entity_etag, odata_error_response, odata_payload
from utils.odata_datetime import date_only_to_odata
from utils.odata_response import odata_collection, odata_entity
from utils.odata_etag import etag_for_datetime, validate_if_match
from utils.odata_query import parse_paging, with_inlinecount
from utils.time import now_utc
from repo.settings_repo import SettingsRepo
from services.settings_service import SettingsService
from services.export_service import ExportService

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
    "DateCheck": "date",
    "TimeCheck": "date",
    "TimeZone": "date",
    "LocationKey": "location_key",
    "Lpc": "lpc",
    "LpcText": "lpc_text",
    "Profession": "observed_position",
    "ProfessionText": "observed_position",
    "Status": "status",
    "ChangedOn": "changed_on",
    "EquipName": "equipment",
}
CHECK_MAP = {"RootKey": "root_id", "ChecksNum": "position", "ChangedOn": "changed_on"}
BARRIER_MAP = {"RootKey": "root_id", "BarriersNum": "position", "ChangedOn": "changed_on"}

_STATUS_TRANSITIONS = {
    "DRAFT": {"SUBMITTED"},
    "SUBMITTED": {"DONE", "REJECTED"},
    "REJECTED": {"DRAFT"},
    "DONE": set(),
}


def _err(status: int, code: str, message: str):
    return odata_error_response(status, code, message)


def _reject_expand(expand: str | None):
    if expand:
        return _err(400, "VALIDATION_ERROR", "EXPAND_NOT_ALLOWED")
    return None


def _hex(raw: str) -> str:
    return str(raw or "").replace("-", "").upper()


def _uuid_from_hex(value: str) -> str:
    h = str(value or "").replace("-", "").lower()
    if len(h) != 32:
        return str(value or "")
    try:
        return str(uuid.UUID(hex=h))
    except ValueError:
        return str(value or "")


def _normalize_hex_key(value: str) -> str:
    return normalize_raw16_hex(value)


def _entity_key(key_expr: str) -> str:
    cleaned = str(key_expr or "").strip().strip("'")
    for prefix in ("Key=", "RootKey="):
        if cleaned.startswith(prefix):
            cleaned = cleaned.split("=", 1)[1].strip().strip("'")
    return _uuid_from_hex(cleaned)


def _resolve_root_from_payload(payload: dict) -> str:
    root_key = str(payload.get("RootKey") or payload.get("root_id") or "")
    if root_key:
        return root_key
    full = payload.get("FullPayload") or payload
    root_block = full.get("root") if isinstance(full, dict) else {}
    return str((root_block or {}).get("Key") or (root_block or {}).get("RootKey") or (root_block or {}).get("id") or "")


def _datecheck_datetime(root: ChecklistRoot) -> datetime:
    raw = (root.date or "").strip()
    if raw:
        try:
            date_part = datetime.strptime(raw[:10], "%Y-%m-%d").date()
            return datetime(date_part.year, date_part.month, date_part.day, tzinfo=timezone.utc)
        except ValueError:
            pass
    changed = root.changed_on if root.changed_on else now_utc()
    changed = changed if changed.tzinfo else changed.replace(tzinfo=timezone.utc)
    return datetime(changed.year, changed.month, changed.day, tzinfo=timezone.utc)


def _dict_text(db: Session, domain: str, key: str) -> str:
    row = db.query(DictionaryItem).filter(DictionaryItem.domain == domain, DictionaryItem.key == str(key or "")).first()
    return row.text if row else ""




def _status_external(status: str | None) -> str:
    raw = str(status or "").upper()
    legacy = {"01": "DRAFT", "02": "SUBMITTED", "03": "DONE"}
    return legacy.get(raw, raw or "DRAFT")


def _normalize_status_input(status: str | None) -> str:
    return _status_external(status)


def _date_ymd_from_any(value) -> str:
    if value is None:
        return ""
    raw = str(value)
    if raw.startswith("/Date(") and raw.endswith(")/"):
        try:
            ms = int(raw[6:-2].split("+")[0].split("-")[0])
            dt = datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
            return dt.strftime("%Y-%m-%d")
        except ValueError:
            return ""
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    if "T" in raw:
        raw = raw.split("T", 1)[0]
    return raw[:10]




def _date_ms_from_any(value) -> int:
    if value is None:
        return 0
    raw = str(value)
    if raw.startswith('/Date(') and raw.endswith(')/'):
        body = raw[6:-2]
        sign_pos = max(body.find('+', 1), body.find('-', 1))
        ms_part = body if sign_pos < 0 else body[:sign_pos]
        try:
            return int(ms_part)
        except ValueError:
            return 0
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    try:
        dt = datetime.fromisoformat(raw.replace('Z', '+00:00'))
        dt = dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
        return int(dt.astimezone(timezone.utc).timestamp() * 1000)
    except Exception:
        return 0


def _build_search_predicate(filter_expr: str | None):
    expr = str(filter_expr or "").strip()
    if not expr:
        return lambda _row: True

    tokens = re.findall(r"substringof\(|contains\(|\(|\)|,|'[^']*'|datetime'[^']*'|/Date\([^)]*\)/|\b(?:and|or|eq|ne|true|false)\b|[A-Za-z_][A-Za-z0-9_]*", expr, flags=re.IGNORECASE)
    idx = 0

    def lit(tok: str):
        low = tok.lower()
        if tok.startswith("'") and tok.endswith("'"):
            return tok[1:-1]
        if low.startswith("datetime'") and tok.endswith("'"):
            return tok
        if tok.startswith("/Date("):
            return tok
        if low in {"true", "false"}:
            return low == "true"
        return tok

    def cmp(field, op, value):
        def _fn(row):
            left = row.get(field)
            right = value
            if field == "DateCheck":
                left = _date_ymd_from_any(left)
                right = _date_ymd_from_any(right)
            elif field == "Status":
                left = str(left or "").upper()
                right = str(right or "").upper()
            else:
                left = str(left or "")
                right = str(right or "")
            return left == right if op == "eq" else left != right
        return _fn

    def parse_expr():
        nonlocal idx
        node = parse_term()
        while idx < len(tokens) and tokens[idx].lower() == "or":
            idx += 1
            rhs = parse_term()
            prev = node
            node = lambda row, a=prev, b=rhs: bool(a(row) or b(row))
        return node

    def parse_term():
        nonlocal idx
        node = parse_factor()
        while idx < len(tokens) and tokens[idx].lower() == "and":
            idx += 1
            rhs = parse_factor()
            prev = node
            node = lambda row, a=prev, b=rhs: bool(a(row) and b(row))
        return node

    def parse_factor():
        nonlocal idx
        if idx < len(tokens) and tokens[idx] == "(":
            idx += 1
            node = parse_expr()
            if idx < len(tokens) and tokens[idx] == ")":
                idx += 1
            return node
        return parse_predicate()

    def parse_predicate():
        nonlocal idx
        tok = tokens[idx]
        low = tok.lower()
        if low in {"substringof(", "contains("}:
            idx += 1
            first = lit(tokens[idx]); idx += 1
            if idx < len(tokens) and tokens[idx] == ",":
                idx += 1
            second = lit(tokens[idx]); idx += 1
            if idx < len(tokens) and tokens[idx] == ")":
                idx += 1
            field = second if low == "contains(" else second
            needle = first if low == "contains(" else first
            fn = lambda row, f=field, n=str(needle or "").lower(): n in str(row.get(f) or "").lower()
            if idx + 1 < len(tokens) and tokens[idx].lower() == "eq":
                idx += 1
                bool_val = bool(lit(tokens[idx])); idx += 1
                return (lambda row, pred=fn, b=bool_val: pred(row) if b else (not pred(row)))
            return fn

        field = tok
        idx += 1
        if idx >= len(tokens):
            return lambda _row: True
        op = tokens[idx].lower(); idx += 1
        if idx >= len(tokens):
            return lambda _row: True
        value = lit(tokens[idx]); idx += 1
        if op not in {"eq", "ne"}:
            return lambda _row: True
        return cmp(field, op, value)

    try:
        return parse_expr()
    except Exception:
        return lambda _row: True


def _apply_orderby_rows(rows: list[dict], orderby: str | None) -> list[dict]:
    if not orderby:
        return rows
    clauses = [x.strip() for x in str(orderby).split(",") if x.strip()]
    sorted_rows = list(rows)
    for clause in reversed(clauses):
        field, _, direction = clause.partition(" ")
        reverse = direction.strip().lower() == "desc"

        def key_fn(item, f=field):
            value = item.get(f)
            if f in {"DateCheck", "ChangedOn", "CreatedOn"}:
                return _date_ms_from_any(value)
            if isinstance(value, str):
                return value.lower()
            return value

        sorted_rows.sort(key=key_fn, reverse=reverse)
    return sorted_rows
def _rate(items, pred) -> float:
    if not items:
        return 1.0
    ok = sum(1 for item in items if pred(item))
    return round(ok / len(items), 4)


def _to_search(root: ChecklistRoot, db: Session | None = None) -> dict:
    profession = root.observed_position or ""
    lpc = root.lpc or ""
    profession_text = _dict_text(db, "PROFESSION", profession) if db else ""
    lpc_text = _dict_text(db, "LPC", lpc) if db else ""
    return {
        "Key": _hex(root.id),
        "Id": root.checklist_id or "",
        "DateCheck": date_only_to_odata(_datecheck_datetime(root).date()),
        "TimeCheck": "12:00:00",
        "TimeZone": "UTC",
        "LocationKey": root.location_key or "",
        "Lpc": lpc,
        "LpcText": lpc_text or lpc,
        "Profession": profession,
        "ProfessionText": profession_text or profession,
        "EquipName": root.equipment or "",
        "Status": _status_external(root.status),
        "ChangedOn": format_datetime(root.changed_on),
        "CreatedOn": format_datetime(root.created_on),
        "RequestId": root.checklist_id or "",
        "HasFailedChecks": any((c.status or "").upper() == "FAIL" for c in root.checks),
        "HasFailedBarriers": any(not bool(b.is_active) for b in root.barriers),
        "SuccessChecksRate": _rate(root.checks, lambda c: (c.status or "").upper() != "FAIL"),
        "SuccessBarriersRate": _rate(root.barriers, lambda b: bool(b.is_active)),
    }


def _to_root(root: ChecklistRoot, db: Session | None = None) -> dict:
    s = _to_search(root, db=db)
    return {
        "Key": s["Key"],
        "RequestId": s["RequestId"],
        "Id": s["Id"],
        "ChangedOn": s["ChangedOn"],
        "CreatedOn": s["CreatedOn"],
        "Status": s["Status"],
        "HasFailedChecks": s["HasFailedChecks"],
        "HasFailedBarriers": s["HasFailedBarriers"],
        "SuccessChecksRate": s["SuccessChecksRate"],
        "SuccessBarriersRate": s["SuccessBarriersRate"],
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
        "DateCheck": date_only_to_odata(_datecheck_datetime(root).date()),
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
    points.extend([
        a.get("changed_on")
        for a in _ATTACHMENTS.values()
        if a.get("root_uuid") == root.id and a.get("changed_on")
    ])
    points = [p for p in points if p is not None]
    return max(points) if points else now_utc()


def _is_expired_lock(expires_on: datetime | None) -> bool:
    if not expires_on:
        return False
    dt = expires_on if expires_on.tzinfo else expires_on.replace(tzinfo=timezone.utc)
    return dt < now_utc()


def _build_lock_status_row(db: Session, root_uuid: str, session_guid: str = "") -> dict:
    active = db.query(LockEntry).filter(LockEntry.pcct_uuid == root_uuid, LockEntry.is_killed.is_(False)).first()
    if not active:
        own = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == root_uuid,
            LockEntry.session_guid == session_guid,
        ).order_by(LockEntry.last_heartbeat.desc()).first() if session_guid else None
        if own:
            if own.is_killed:
                return {"RootKey": _hex(root_uuid), "Ok": False, "ReasonCode": "KILLED", "Owner": own.user_id or "", "ExpiresOn": format_datetime(own.expires_at)}
            if _is_expired_lock(own.expires_at):
                own.is_killed = True
                db.commit()
                return {"RootKey": _hex(root_uuid), "Ok": False, "ReasonCode": "EXPIRED", "Owner": own.user_id or "", "ExpiresOn": format_datetime(own.expires_at)}
        return {"RootKey": _hex(root_uuid), "Ok": True, "ReasonCode": "FREE", "Owner": "", "ExpiresOn": None}
    if _is_expired_lock(active.expires_at):
        active.is_killed = True
        db.commit()
        return {"RootKey": _hex(root_uuid), "Ok": False, "ReasonCode": "EXPIRED", "Owner": active.user_id or "", "ExpiresOn": format_datetime(active.expires_at)}
    if active.session_guid == session_guid and session_guid:
        return {"RootKey": _hex(root_uuid), "Ok": True, "ReasonCode": "OWNED_BY_YOU", "Owner": active.user_id, "ExpiresOn": format_datetime(active.expires_at)}
    return {"RootKey": _hex(root_uuid), "Ok": False, "ReasonCode": "LOCKED_BY_OTHER", "Owner": active.user_id, "ExpiresOn": format_datetime(active.expires_at)}


def _parse_odata_datetime(value: str | None) -> datetime:
    if not value:
        return now_utc()
    raw = str(value).strip()
    if raw.startswith("/Date(") and raw.endswith(")/"):
        body = raw[6:-2]
        sign_pos = max(body.find("+", 1), body.find("-", 1))
        millis_part = body if sign_pos < 0 else body[:sign_pos]
        try:
            return datetime.fromtimestamp(int(millis_part) / 1000, tz=timezone.utc)
        except ValueError:
            return now_utc()
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    return datetime.fromisoformat(raw.replace("Z", "+00:00"))


def _apply_select(row: dict, select_expr: str | None) -> dict:
    if not select_expr:
        return row
    keep = {x.strip() for x in str(select_expr).split(",") if x.strip()}
    return {k: v for k, v in row.items() if k in keep}


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


def _normalize_filter_hex_keys(filter_expr: str | None, fields: tuple[str, ...] = ("Key", "RootKey")) -> str | None:
    if not filter_expr:
        return filter_expr

    out = str(filter_expr)
    for field in fields:
        rx = re.compile(rf"\b{re.escape(field)}\b\s+(eq|ne)\s+'([0-9a-fA-F\-]{{32,36}})'", re.IGNORECASE)

        def _replace(match):
            op = match.group(1)
            raw = match.group(2)
            normalized = _entity_key(raw)
            return f"{field} {op} '{normalized}'"

        out = rx.sub(_replace, out)
    return out


def _if_match_check(if_match: str | None, agg: datetime):
    if not if_match or if_match == "*":
        return
    if not validate_if_match(if_match, etag_for_datetime(agg)):
        raise ValueError("PRECONDITION_FAILED")


def _optimistic_check(client_agg: str | None, root: ChecklistRoot):
    if not client_agg:
        raise ValueError("VALIDATION_ERROR")
    cms = int(_parse_odata_datetime(client_agg).timestamp() * 1000)
    current = int(_agg_changed_on(root).timestamp() * 1000)
    if cms != current:
        raise ValueError("CONFLICT")


def _load_root_or_error(db: Session, root_key: str):
    try:
        _normalize_hex_key(root_key)
    except ValueError:
        return None, _err(400, "VALIDATION_ERROR", "RootKey must be RAW16 HEX (32 chars)")
    root = db.query(ChecklistRoot).filter(
        ChecklistRoot.id == _entity_key(root_key),
        ChecklistRoot.is_deleted.isnot(True),
    ).first()
    if not root:
        return None, _err(404, "NOT_FOUND", "Checklist not found")
    return root, None


def _apply_change(db: Session, root: ChecklistRoot, change: dict):
    entity = str(change.get("Entity") or "").upper()
    mode = str(change.get("EditMode") or "U").upper()
    fields = change.get("Fields") or {}
    key = _entity_key(str(change.get("Key") or ""))
    updated_mapping = None
    row_changed_on = now_utc()

    if entity == "ROOT" and mode == "U":
        for k, v in fields.items():
            attr = ROOT_MAP.get(k)
            if attr and hasattr(root, attr):
                setattr(root, attr, v)
        root.changed_on = row_changed_on
    elif entity == "BASIC" and mode == "U":
        root.location_key = fields.get("LocationKey", root.location_key)
        root.location_name = fields.get("LocationName", root.location_name)
        root.equipment = fields.get("EquipName", root.equipment)
        root.lpc = fields.get("Lpc", root.lpc)
        root.observed_position = fields.get("Profession", root.observed_position)
        if fields.get("DateCheck"):
            dt = _parse_odata_datetime(fields.get("DateCheck"))
            root.date = dt.date().isoformat()
        root.changed_on = row_changed_on
    elif entity == "CHECK":
        if mode == "C":
            created = ChecklistCheck(id=str(uuid.uuid4()), root_id=root.id, text=fields.get("Comment", ""), status="PASS" if fields.get("Result", True) else "FAIL", position=int(fields.get("ChecksNum", 0)), changed_on=row_changed_on)
            db.add(created)
            updated_mapping = {"Entity": "CHECK", "ClientKey": change.get("Key") or "", "ServerKey": _hex(created.id)}
        elif mode == "U":
            row = db.query(ChecklistCheck).filter(ChecklistCheck.id == key).first()
            if row:
                if "Comment" in fields:
                    row.text = fields["Comment"]
                if "Result" in fields:
                    row.status = "PASS" if fields["Result"] else "FAIL"
                row.changed_on = row_changed_on
        elif mode == "D":
            db.query(ChecklistCheck).filter(ChecklistCheck.id == key).delete()
    elif entity == "BARRIER":
        if mode == "C":
            created = ChecklistBarrier(id=str(uuid.uuid4()), root_id=root.id, description=fields.get("Comment", ""), is_active=bool(fields.get("Result", True)), position=int(fields.get("BarriersNum", 0)), changed_on=row_changed_on)
            db.add(created)
            updated_mapping = {"Entity": "BARRIER", "ClientKey": change.get("Key") or "", "ServerKey": _hex(created.id)}
        elif mode == "U":
            row = db.query(ChecklistBarrier).filter(ChecklistBarrier.id == key).first()
            if row:
                if "Comment" in fields:
                    row.description = fields["Comment"]
                if "Result" in fields:
                    row.is_active = bool(fields["Result"])
                row.changed_on = row_changed_on
        elif mode == "D":
            db.query(ChecklistBarrier).filter(ChecklistBarrier.id == key).delete()

    return updated_mapping


def _validate_status_change(root: ChecklistRoot, new_status: str):
    old_status = _status_external(root.status)
    if old_status == new_status:
        return
    allowed = _STATUS_TRANSITIONS.get(old_status)
    if allowed is not None and new_status not in allowed:
        raise ValueError("VALIDATION_ERROR")


@router.api_route(f"{SERVICE_ROOT}/", methods=["GET", "HEAD"])
def service_document():
    return odata_entity({"EntitySets": [
        "ChecklistSearchSet", "ChecklistRootSet", "ChecklistBasicInfoSet", "ChecklistCheckSet", "ChecklistBarrierSet",
        "DictionaryItemSet", "LastChangeSet", "LockStatusSet", "AttachmentFolderSet", "AttachmentSet", "RuntimeSettingsSet",
    ]})


@router.get(f"{SERVICE_ROOT}/$metadata")
def metadata():
    return Response(content=build_metadata(), media_type="application/xml; charset=utf-8")


@router.get(f"{SERVICE_ROOT}/ChecklistSearchSet")
@router.get(f"{SERVICE_ROOT}/ChecklistRoots")
@router.get(f"{SERVICE_ROOT}/SearchRows")
@router.get("/ChecklistRoots")
@router.get("/SearchRows")
def checklist_search_set(
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
    roots = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)).all()
    mapped = [_to_search(r, db=db) for r in roots]
    pred = parse_filter_to_predicate(filter)
    mapped = [row for row in mapped if pred(row)]
    mapped = _apply_orderby_rows(mapped, orderby)
    top, skip = parse_paging(top, skip, DEFAULT_PAGE_SIZE)
    total = len(mapped)
    paged = mapped[skip: skip + top]
    return odata_collection([_apply_select(r, select) for r in paged], with_inlinecount(inlinecount, total))


@router.get(f"{SERVICE_ROOT}/ChecklistSearchSet/$count")
def checklist_search_count(filter: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    roots = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)).all()
    mapped = [_to_search(r, db=db) for r in roots]
    pred = parse_filter_to_predicate(filter)
    return Response(content=str(len([r for r in mapped if pred(r)])), media_type="text/plain")


@router.get(f"{SERVICE_ROOT}/ChecklistRootSet")
def checklist_root_set(filter: str | None = Query(None, alias="$filter"), orderby: str | None = Query(None, alias="$orderby"), top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), select: str | None = Query(None, alias="$select"), db: Session = Depends(get_db)):
    filter = _normalize_filter_hex_keys(filter, fields=("Key",))
    rows, total = _apply_order_filter(db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)), ChecklistRoot, ROOT_MAP, filter, orderby, top, skip)
    return odata_payload([_apply_select(_to_root(r, db=db), select) for r in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistRootSet({{entity_key}})")
def checklist_root_entity(entity_key: str, response: Response, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    response.headers["ETag"] = format_entity_etag(_agg_changed_on(root))
    return odata_entity(_to_root(root, db=db))


@router.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet")
def checklist_basic_info_set(filter: str | None = Query(None, alias="$filter"), top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    filter = _normalize_filter_hex_keys(filter, fields=("RootKey",))
    rows, total = _apply_order_filter(db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)), ChecklistRoot, {"RootKey": "id"}, filter, None, top, skip)
    return odata_payload([_to_basic(r) for r in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet({{entity_key}})")
def checklist_basic_info_entity(entity_key: str, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    return odata_entity(_to_basic(root))


@router.get(f"{SERVICE_ROOT}/ChecklistCheckSet")
def checklist_check_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), top: int = Query(20, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    filter = _normalize_filter_hex_keys(filter, fields=("RootKey", "Key"))
    rows, total = _apply_order_filter(db.query(ChecklistCheck), ChecklistCheck, CHECK_MAP, filter, None, top, skip)
    return odata_payload([_to_check(c) for c in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistBarrierSet")
def checklist_barrier_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), top: int = Query(20, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    filter = _normalize_filter_hex_keys(filter, fields=("RootKey", "Key"))
    rows, total = _apply_order_filter(db.query(ChecklistBarrier), ChecklistBarrier, BARRIER_MAP, filter, None, top, skip)
    return odata_payload([_to_barrier(c) for c in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/DictionaryItemSet")
def dictionary_item_set(filter: str | None = Query(None, alias="$filter"), top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    rows, total = _apply_order_filter(db.query(DictionaryItem), DictionaryItem, {"Domain": "domain", "Key": "key", "Text": "text"}, filter, None, top, skip)
    return odata_payload([{"Domain": r.domain, "Key": r.key, "Text": r.text} for r in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/RuntimeSettingsSet")
def runtime_settings_set(db: Session = Depends(get_db)):
    item = SettingsService.load_global(SettingsRepo(db))
    return odata_collection([item])


@router.get(f"{SERVICE_ROOT}/RuntimeSettingsSet({{entity_key}})")
def runtime_settings_entity(entity_key: str, db: Session = Depends(get_db)):
    cleaned = str(entity_key or "").strip()
    if cleaned.startswith("Key="):
        cleaned = cleaned.split("=", 1)[1]
    cleaned = cleaned.strip("\'\"")
    if cleaned and cleaned.upper() != "GLOBAL":
        return _err(404, "NOT_FOUND", "Runtime settings not found")
    rows = runtime_settings_set(db)
    return odata_entity((rows.get("d", {}).get("results") or [])[0])


@router.get(f"{SERVICE_ROOT}/LastChangeSet({{entity_key}})")
def last_change_entity(entity_key: str, response: Response, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    agg = _agg_changed_on(root)
    response.headers["ETag"] = format_entity_etag(agg)
    return odata_entity({"RootKey": _hex(root.id), "AggChangedOn": format_datetime(agg)})


@router.get(f"{SERVICE_ROOT}/LastChangeSet")
def last_change_set(
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    filter = _normalize_filter_hex_keys(filter, fields=("RootKey",))
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"RootKey": "id"},
        filter,
        orderby,
        top,
        skip,
    )
    payload = [{"RootKey": _hex(r.id), "AggChangedOn": format_datetime(_agg_changed_on(r))} for r in rows]
    return odata_payload(payload, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/LockStatusSet")
def lock_status_set(
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    session_guid: str = Query("", alias="SessionGuid"),
    db: Session = Depends(get_db),
):
    filter = _normalize_filter_hex_keys(filter, fields=("RootKey",))
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"RootKey": "id"},
        filter,
        orderby,
        top,
        skip,
    )
    payload = [_build_lock_status_row(db, r.id, session_guid=session_guid) for r in rows]
    return odata_payload(payload, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/LockStatusSet({{entity_key}})")
def lock_status_entity(entity_key: str, session_guid: str = Query("", alias="SessionGuid"), db: Session = Depends(get_db)):
    return odata_entity(_build_lock_status_row(db, _entity_key(entity_key), session_guid=session_guid))


@router.post(f"{SERVICE_ROOT}/LockControl")
@router.post("/actions/LockControl")
def lock_control(payload: dict, db: Session = Depends(get_db)):
    action = str(payload.get("Action") or "").upper()
    root_key = str(payload.get("RootKey") or "")
    session = str(payload.get("SessionGuid") or "")
    uname = str(payload.get("Uname") or "ANON")
    if not root_key:
        return _err(400, "VALIDATION_ERROR", "RootKey is required")
    root_uuid = _entity_key(root_key)
    root_exists = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_uuid, ChecklistRoot.is_deleted.isnot(True)).first()
    if not root_exists:
        return _err(404, "NOT_FOUND", "Checklist not found")
    if action == "ACQUIRE":
        r = LockService.acquire(db, root_uuid, session, uname, payload.get("StealFrom"))
        ok = bool(r.get("success"))
        return odata_entity({"Ok": ok, "ReasonCode": "OWNED_BY_YOU" if ok else "LOCKED_BY_OTHER", "ExpiresOn": format_datetime(r.get("lock_expires")), "IsKilled": bool(r.get("is_killed_flag"))})
    if action == "HEARTBEAT":
        try:
            r = LockService.heartbeat(db, root_uuid, session)
            ok = bool(r.get("success"))
            return odata_entity({"Ok": ok, "ReasonCode": "OWNED_BY_YOU" if ok else "KILLED", "ExpiresOn": format_datetime(r.get("lock_expires")), "IsKilled": bool(r.get("is_killed"))})
        except ValueError as exc:
            code = str(exc)
            if code == "NO_LOCK":
                return odata_entity({"Ok": False, "ReasonCode": "FREE", "ExpiresOn": None, "IsKilled": False})
            return _err(410, "LOCK_EXPIRED", "Lock expired")
    if action == "RELEASE":
        r = LockService.release(db, root_uuid, session)
        released = bool(r.get("released"))
        return odata_entity({"Ok": released, "ReasonCode": "FREE", "ExpiresOn": None, "IsKilled": False})
    return _err(400, "VALIDATION_ERROR", "Unsupported Action")


@router.post("/actions/LockAcquire")
def lock_acquire_action_alias(
    root_id: str | None = Query(None),
    session_guid: str | None = Query(None),
    user_id: str | None = Query(None),
    iv_steal_from: str | None = Query(None),
    payload: dict | None = None,
    db: Session = Depends(get_db),
):
    body = payload or {}
    req = {
        "Action": "ACQUIRE",
        "RootKey": body.get("RootKey") or body.get("root_id") or root_id,
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Uname": body.get("Uname") or body.get("uname") or body.get("user_id") or user_id or "ANON",
        "StealFrom": body.get("StealFrom") or body.get("iv_steal_from") or iv_steal_from,
    }
    return lock_control(req, db)


@router.post("/actions/LockHeartbeat")
def lock_heartbeat_action_alias(
    root_id: str | None = Query(None),
    session_guid: str | None = Query(None),
    payload: dict | None = None,
    db: Session = Depends(get_db),
):
    body = payload or {}
    req = {
        "Action": "HEARTBEAT",
        "RootKey": body.get("RootKey") or body.get("root_id") or root_id,
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Uname": body.get("Uname") or body.get("uname") or body.get("user_id") or "ANON",
    }
    return lock_control(req, db)


@router.post("/actions/LockRelease")
def lock_release_action_alias(
    root_id: str | None = Query(None),
    session_guid: str | None = Query(None),
    payload: dict | None = None,
    db: Session = Depends(get_db),
):
    body = payload or {}
    req = {
        "Action": "RELEASE",
        "RootKey": body.get("RootKey") or body.get("root_id") or root_id,
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Uname": body.get("Uname") or body.get("uname") or body.get("user_id") or "ANON",
    }
    return lock_control(req, db)


@router.post("/actions/LockStatus")
def lock_status_action_alias(
    root_id: str | None = Query(None),
    session_guid: str | None = Query(None),
    payload: dict | None = None,
    db: Session = Depends(get_db),
):
    body = payload or {}
    root = body.get("RootKey") or body.get("root_id") or root_id
    sess = body.get("SessionGuid") or body.get("session_guid") or session_guid or ""
    if not root:
        return _err(400, "VALIDATION_ERROR", "RootKey is required")
    return odata_entity(_build_lock_status_row(db, _entity_key(str(root)), session_guid=str(sess)))


# Legacy /lock/* aliases routed to canonical lock semantics for migration-safe behavior.
@router.post("/lock/acquire")
@router.post(f"{SERVICE_ROOT}/lock/acquire")
def lock_acquire_alias(payload: dict | None = None, db: Session = Depends(get_db), object_uuid: str | None = Query(None), session_guid: str | None = Query(None), uname: str | None = Query(None), iv_steal_from: str | None = Query(None)):
    body = payload or {}
    req = {
        "Action": "ACQUIRE",
        "RootKey": body.get("RootKey") or body.get("root_id") or body.get("object_uuid") or object_uuid,
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Uname": body.get("Uname") or body.get("uname") or uname or "ANON",
        "StealFrom": body.get("StealFrom") or body.get("iv_steal_from") or iv_steal_from,
    }
    return lock_control(req, db)


@router.post("/lock/heartbeat")
@router.post(f"{SERVICE_ROOT}/lock/heartbeat")
def lock_heartbeat_alias(payload: dict | None = None, db: Session = Depends(get_db), object_uuid: str | None = Query(None), session_guid: str | None = Query(None), uname: str | None = Query(None)):
    body = payload or {}
    req = {
        "Action": "HEARTBEAT",
        "RootKey": body.get("RootKey") or body.get("root_id") or body.get("object_uuid") or object_uuid,
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Uname": body.get("Uname") or body.get("uname") or uname or "ANON",
    }
    return lock_control(req, db)


@router.post("/lock/release")
@router.post(f"{SERVICE_ROOT}/lock/release")
def lock_release_alias(payload: dict | None = None, db: Session = Depends(get_db), object_uuid: str | None = Query(None), session_guid: str | None = Query(None), uname: str | None = Query(None)):
    body = payload or {}
    req = {
        "Action": "RELEASE",
        "RootKey": body.get("RootKey") or body.get("root_id") or body.get("object_uuid") or object_uuid,
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Uname": body.get("Uname") or body.get("uname") or uname or "ANON",
    }
    return lock_control(req, db)


@router.get("/lock/status")
@router.get(f"{SERVICE_ROOT}/lock/status")
def lock_status_alias(object_uuid: str = Query(...), session_guid: str = Query(""), db: Session = Depends(get_db)):
    row = _build_lock_status_row(db, _entity_key(object_uuid), session_guid=session_guid)
    return {
        "success": bool(row.get("Ok")),
        "is_killed": str(row.get("ReasonCode") or "") == "KILLED",
        "lock_expires": row.get("ExpiresOn"),
        "reason_code": row.get("ReasonCode"),
        "owner": row.get("Owner") or "",
        "d": row,
    }


@router.post(f"{SERVICE_ROOT}/AutoSave")
@router.post("/actions/AutoSave")
@router.post("/actions/AutoSaveChecklist")
def auto_save(payload: dict, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, str(payload.get("RootKey") or payload.get("root_id") or ""))
    if err:
        return err
    try:
        _if_match_check(if_match, _agg_changed_on(root))
        _optimistic_check(payload.get("ClientAggChangedOn"), root)
    except ValueError as ex:
        if str(ex) == "PRECONDITION_FAILED":
            return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
        if str(ex) == "VALIDATION_ERROR":
            return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
        return _err(409, "CONFLICT", "AggChangedOn mismatch")

    mappings = []
    for change in payload.get("Changes") or []:
        mapped = _apply_change(db, root, change)
        if mapped:
            mappings.append(mapped)
    root.changed_on = now_utc()
    db.commit()
    agg = _agg_changed_on(root)
    return odata_entity({"RootKey": _hex(root.id), "AggChangedOn": format_datetime(agg), "UpdatedKeysMapping": mappings})


@router.post(f"{SERVICE_ROOT}/SaveChanges")
@router.post("/actions/SaveChanges")
@router.post("/actions/SaveChecklist")
def save_changes(payload: dict, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, _resolve_root_from_payload(payload))
    if err:
        return err
    try:
        _if_match_check(if_match, _agg_changed_on(root))
        _optimistic_check(payload.get("ClientAggChangedOn"), root)
    except ValueError as ex:
        if str(ex) == "PRECONDITION_FAILED":
            return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
        if str(ex) == "VALIDATION_ERROR":
            return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
        return _err(409, "CONFLICT", "AggChangedOn mismatch")

    full = payload.get("FullPayload") or payload
    for k, v in (full.get("root") or {}).items():
        attr = ROOT_MAP.get(k)
        if attr and hasattr(root, attr):
            setattr(root, attr, v)
    basic = full.get("basic") or {}
    root.location_key = basic.get("LocationKey", root.location_key)
    root.location_name = basic.get("LocationName", root.location_name)
    root.equipment = basic.get("EquipName", root.equipment)
    root.lpc = basic.get("Lpc", root.lpc)
    root.observed_position = basic.get("Profession", root.observed_position)
    if basic.get("DateCheck"):
        dt = _parse_odata_datetime(basic.get("DateCheck"))
        root.date = dt.date().isoformat()

    db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root.id).delete()
    db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root.id).delete()
    for c in full.get("checks") or []:
        db.add(ChecklistCheck(id=str(uuid.uuid4()), root_id=root.id, text=c.get("Comment", ""), status="PASS" if c.get("Result", True) else "FAIL", position=int(c.get("ChecksNum", 0))))
    for b in full.get("barriers") or []:
        db.add(ChecklistBarrier(id=str(uuid.uuid4()), root_id=root.id, description=b.get("Comment", ""), is_active=bool(b.get("Result", True)), position=int(b.get("BarriersNum", 0))))

    root.changed_on = now_utc()
    db.commit()
    db.refresh(root)
    out = _to_root(root, db=db)
    out["RootKey"] = out["Key"]
    out["AggChangedOn"] = format_datetime(_agg_changed_on(root))
    return odata_entity(out)


@router.post(f"{SERVICE_ROOT}/SetChecklistStatus")
@router.post("/actions/SetChecklistStatus")
def set_status(payload: dict, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, _resolve_root_from_payload(payload))
    if err:
        return err
    try:
        _if_match_check(if_match, _agg_changed_on(root))
        _optimistic_check(payload.get("ClientAggChangedOn"), root)
    except ValueError as ex:
        if str(ex) == "PRECONDITION_FAILED":
            return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
        if str(ex) == "VALIDATION_ERROR":
            return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
        return _err(409, "CONFLICT", "AggChangedOn mismatch")

    new_status = _normalize_status_input(payload.get("NewStatus") or payload.get("new_status") or "")
    if new_status not in {"DRAFT", "SUBMITTED", "DONE", "REJECTED"}:
        return _err(400, "VALIDATION_ERROR", "Unsupported status")
    try:
        _validate_status_change(root, new_status)
    except ValueError:
        return _err(400, "VALIDATION_ERROR", "Invalid status transition")
    root.status = new_status
    root.changed_on = now_utc()
    db.commit()
    return odata_entity({"RootKey": _hex(root.id), "Status": root.status, "AggChangedOn": format_datetime(_agg_changed_on(root))})


@router.get(f"{SERVICE_ROOT}/GetHierarchy")
@router.post(f"{SERVICE_ROOT}/GetHierarchy")
@router.post("/actions/GetMplHierarchy")
async def get_hierarchy(request: Request, date_check: str | None = Query(None, alias="DateCheck"), method: str = Query("location_tree", alias="Method"), db: Session = Depends(get_db)):
    payload = {}
    if request.method == "POST":
        try:
            payload = await request.json()
        except Exception:
            payload = {}
    date_value = payload.get("DateCheck") or payload.get("date") or date_check
    method_value = str(payload.get("Method") or payload.get("method") or method or "location_tree").lower()
    if method_value != "location_tree":
        return _err(400, "VALIDATION_ERROR", "Unsupported hierarchy method")
    try:
        dt = _parse_odata_datetime(date_value)
    except Exception:
        dt = now_utc()
    nodes = HierarchyService.get_tree(db, dt.date())
    return odata_entity({"Method": "location_tree", "DateCheck": format_datetime(datetime(dt.year, dt.month, dt.day, tzinfo=timezone.utc)), "results": nodes})


@router.post(f"{SERVICE_ROOT}/ReportExport")
@router.post("/actions/export")
def report_export(payload: dict, db: Session = Depends(get_db)):
    root_keys = payload.get("RootKeys") or payload.get("keys") or payload.get("ids") or []
    rows = []
    for key in root_keys:
        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(str(key))).first()
        if not root:
            continue
        base = _to_search(root, db=db)
        checks = [_to_check(c) for c in root.checks]
        barriers = [_to_barrier(b) for b in root.barriers]
        if not checks and not barriers:
            rows.append({"RootKey": base["Key"], "Id": base["Id"], "Lpc": base["Lpc"], "Profession": base["Profession"], "DateCheck": base["DateCheck"], "Num": None, "Text": "", "Comment": "", "Result": None})
        for c in checks:
            txt = db.query(DictionaryItem).filter(DictionaryItem.domain == "CHECK", DictionaryItem.key == str(c["ChecksNum"])).first()
            rows.append({"RootKey": base["Key"], "Id": base["Id"], "Lpc": base["Lpc"], "Profession": base["Profession"], "DateCheck": base["DateCheck"], "Num": c["ChecksNum"], "Text": txt.text if txt else "", "Comment": c["Comment"], "Result": c["Result"]})
        for b in barriers:
            txt = db.query(DictionaryItem).filter(DictionaryItem.domain == "BARRIER", DictionaryItem.key == str(b["BarriersNum"])).first()
            rows.append({"RootKey": base["Key"], "Id": base["Id"], "Lpc": base["Lpc"], "Profession": base["Profession"], "DateCheck": base["DateCheck"], "Num": b["BarriersNum"], "Text": txt.text if txt else "", "Comment": b["Comment"], "Result": b["Result"]})
    return odata_collection(rows)


@router.get(f"{SERVICE_ROOT}/AttachmentFolderSet")
def attachment_folder_set(filter: str | None = Query(None, alias="$filter")):
    folders = [
        {
            "FolderKey": k,
            "RootKey": v["root_key"],
            "Title": v.get("title", "General"),
            "CreatedOn": format_datetime(v.get("created_on")),
            "ChangedOn": format_datetime(v.get("changed_on")),
        }
        for k, v in _ATTACHMENTS.items()
        if v.get("is_folder")
    ]
    return odata_payload(folders)


@router.get(f"{SERVICE_ROOT}/AttachmentSet")
def attachment_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand")):
    if (err := _reject_expand(expand)):
        return err
    rows = []
    for key, meta in _ATTACHMENTS.items():
        if meta.get("is_folder"):
            continue
        rows.append({
            "Key": key,
            "AttachmentKey": key,
            "RootKey": meta.get("root_key"),
            "FolderKey": meta.get("folder_key"),
            "CategoryKey": meta.get("category_key") or "GEN",
            "FileName": meta.get("file_name") or "",
            "MimeType": meta.get("mime") or "application/octet-stream",
            "FileSize": int(meta.get("size") or 0),
            "ScanStatus": "OK",
            "ScannedOn": format_datetime(meta.get("changed_on")),
            "CreatedOn": format_datetime(meta.get("created_on")),
            "ChangedOn": format_datetime(meta.get("changed_on")),
        })
    return odata_payload(rows)


@router.put(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})/$value")
async def attachment_value_put(entity_key: str, request: Request, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    payload = await request.body()
    content_type = request.headers.get("content-type") or "application/octet-stream"
    file_name = request.headers.get("Slug") or f"{key}.bin"
    root_key = request.headers.get("X-RootKey") or ""
    root_uuid = ""
    if root_key:
        try:
            _normalize_hex_key(root_key)
            root_uuid = _entity_key(root_key)
        except ValueError:
            return _err(400, "VALIDATION_ERROR", "X-RootKey must be RAW16 HEX (32 chars)")
    now = now_utc()

    file_path = _UPLOAD_DIR / key
    file_path.write_bytes(payload)
    _ATTACHMENTS[key] = {
        "root_key": _hex(root_uuid),
        "root_uuid": root_uuid,
        "folder_key": "",
        "category_key": request.headers.get("X-CategoryKey") or "GEN",
        "file_name": file_name,
        "mime": content_type,
        "size": len(payload),
        "created_on": _ATTACHMENTS.get(key, {}).get("created_on") or now,
        "changed_on": now,
        "path": str(file_path),
    }
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_uuid).first()
    if root:
        root.changed_on = now_utc()
        db.commit()
    return Response(status_code=204)


@router.get(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})/$value")
def attachment_value_get(entity_key: str):
    key = _entity_key(entity_key)
    meta = _ATTACHMENTS.get(key)
    if not meta:
        return _err(404, "NOT_FOUND", "Attachment not found")
    path = Path(meta.get("path") or "")
    if not path.exists():
        return _err(404, "NOT_FOUND", "Attachment content missing")
    return Response(content=path.read_bytes(), media_type=meta.get("mime") or "application/octet-stream")


@router.delete(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})")
def attachment_delete(entity_key: str, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    meta = _ATTACHMENTS.pop(key, None)
    if not meta:
        return _err(404, "NOT_FOUND", "Attachment not found")
    path = Path(meta.get("path") or "")
    if path.exists():
        path.unlink()
    root_uuid = meta.get("root_uuid")
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_uuid).first() if root_uuid else None
    if root:
        root.changed_on = now_utc()
        db.commit()
    return Response(status_code=204)
