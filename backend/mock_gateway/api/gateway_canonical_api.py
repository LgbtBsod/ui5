import base64
import uuid
import re
import json
import mimetypes
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Callable
from urllib.parse import unquote

from fastapi import APIRouter, Depends, Header, Query, Request, Response
from sqlalchemy import asc, desc
from sqlalchemy.orm import Session

from config import DEFAULT_PAGE_SIZE, LOCK_TTL, MAX_PAGE_SIZE
from database import get_db
from models import AttachmentEntry, ChecklistBarrier, ChecklistCheck, ChecklistRoot, DictionaryItem, FrontendRuntimeSettings, LockEntry, Person
from services.authorization_service import AuthorizationService
from services.hierarchy_service import HierarchyService
from services.analytics_service import AnalyticsService
from services.checklist_service import ChecklistService, resolve_bukrs_from_observer
from services.current_user_service import CurrentUserService
from services.lock_service import LockService
from services.metadata_cache import get_metadata, metadata_refreshed_at_iso
from utils.filter_parser import FilterParser
from utils.filter_engine import parse_filter_to_predicate
from utils.filter_errors import FilterSyntaxError
from utils.odata import SERVICE_ROOT, format_datetime, format_entity_etag, odata_error_response, odata_payload
from utils.odata_datetime import date_only_to_odata
from utils.odata_response import odata_collection, odata_entity
from utils.odata_etag import etag_for_datetime, validate_if_match
from utils.odata_query import parse_paging, with_inlinecount
from utils.time import now_utc
from utils.sap_message import build_sap_message
from repo.settings_repo import SettingsRepo
from services.settings_service import DEFAULT_FRONTEND_VARIABLES, SettingsService
from api.gateway_operations import (
    _validate_attachment_upload, _persist_attachment_media, _apply_attachment_metadata,
    _apply_save_attachments, _apply_root_payload, _replace_detail_rows,
    _normalize_attachment_key, _normalize_attachment_category_key, _normalize_attachment_payload_rows,
    _normalize_upload_list, _resolve_upload_mime, _dict_text,
    _hex, _normalize_hex_key, _media_upload_payload
)
from api.gateway_validators import (
    _pick_text, _pick_bool, _coerce_int, _date_ymd_from_any,
    _status_external, _pick_first_present, _normalize_basic_payload,
    _apply_basic_payload, _next_checklist_id, _normalize_status_input
)
from api.gateway_helpers import (
    PayloadExtractor, DateParser, BoundaryResolver, ODataSerializer, FilterMatcher, DictionaryCache, Aggregator
)
from utils.common_helpers import parse_date_ymd, parse_date_ms

router = APIRouter(tags=["GatewayCanonical"])

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
    "TimeCheck": "time_check",
    "TimeZone": "time_zone",
    "LocationKey": "location_key",
    "Lpc": "lpc",
    "LpcText": "lpc_text",
    "Profession": "observed_position",
    "ProfessionText": "observed_position",
    "Bukrs": "bukrs",
    "Status": "status",
    "ChangedOn": "changed_on",
    "EquipName": "equipment",
}
CHECK_MAP = {"Key": "id", "ParentKey": "root_id", "PARENT_KEY": "root_id", "ChecksNum": "position", "ChangedOn": "changed_on"}
BARRIER_MAP = {"Key": "id", "ParentKey": "root_id", "PARENT_KEY": "root_id", "BarriersNum": "position", "ChangedOn": "changed_on"}


@dataclass(frozen=True)
class _DetailKind:
    """Everything that distinguishes a ChecklistCheck row from a ChecklistBarrier row.

    The three call sites that mutate check/barrier rows (SaveChanges/AutoSave batch
    upsert, the legacy _apply_change single-record import, and the ChecklistCheckSet/
    ChecklistBarrierSet REST routes) each used to carry an independent, hand-duplicated
    copy of "Check has text/comment/status(PASS|FAIL)/position, Barrier has
    description/comment/is_active/position". This is the single place that mapping lives;
    the three call sites differ only in how they parse their own input into `to_kwargs`/
    `apply_update`'s normalized dict, not in what a Check or Barrier row looks like.
    """

    model: type
    entity_tag: str  # "CHECK" / "BARRIER", as used by the legacy _apply_change import
    not_found_message: str
    field_map: dict  # $filter/$orderby field map for the ChecklistCheckSet/BarrierSet REST list route
    key_aliases: tuple[str, ...]  # batch-upsert row key field names, checked in order
    text_aliases: tuple[str, ...]  # batch-upsert row text field names, checked in order
    number_aliases: tuple[str, ...]  # batch-upsert row position field names, checked in order
    number_field: str  # canonical PascalCase wire field name: "ChecksNum" / "BarriersNum"
    to_kwargs: Callable[[dict], dict]
    apply_update: Callable[[object, dict], None]
    serialize: Callable[..., dict]


def _check_kwargs(normalized: dict) -> dict:
    return {
        "text": normalized.get("text", ""),
        "comment": normalized.get("comment", ""),
        "status": "PASS" if normalized.get("result", True) else "FAIL",
        "position": normalized.get("position", 0),
    }


def _apply_check_update(row: ChecklistCheck, normalized: dict) -> None:
    if "text" in normalized:
        row.text = normalized["text"]
    if "comment" in normalized:
        row.comment = normalized["comment"]
    if "result" in normalized:
        row.status = "PASS" if normalized["result"] else "FAIL"
    if "position" in normalized:
        row.position = normalized["position"]


def _barrier_kwargs(normalized: dict) -> dict:
    return {
        "description": normalized.get("text", ""),
        "comment": normalized.get("comment", ""),
        "is_active": bool(normalized.get("result", True)),
        "position": normalized.get("position", 0),
    }


def _apply_barrier_update(row: ChecklistBarrier, normalized: dict) -> None:
    if "text" in normalized:
        row.description = normalized["text"]
    if "comment" in normalized:
        row.comment = normalized["comment"]
    if "result" in normalized:
        row.is_active = bool(normalized["result"])
    if "position" in normalized:
        row.position = normalized["position"]
PERSON_VH_MAP = {
    "Pernr": "perner",
    "FirstName": "first_name",
    "LastName": "last_name",
    "MiddleName": "middle_name",
    "Position": "position",
    "OrgUnit": "org_unit",
    "IntegrationName": "integration_name",
    "Begda": "begda",
    "Endda": "endda",
    "ChangedOn": "changed_on",
}

_STATUS_TRANSITIONS = {
    "DRAFT": {"REGISTERED"},
    "REGISTERED": {"CLOSED", "DRAFT"},
    "CLOSED": set(),
}


def _err(status: int, code: str, message: str):
    return odata_error_response(status, code, message)


def _reject_expand(expand: str | None):
    if expand:
        return _err(400, "VALIDATION_ERROR", "EXPAND_NOT_ALLOWED")
    return None


def _entity_key(key_expr: str) -> str:
    """Wrapper for BoundaryResolver.resolve_key for backwards compatibility."""
    return BoundaryResolver.resolve_key(key_expr)


def _boundary_root_key(payload: dict | None = None, *candidates) -> str:
    """Wrapper for BoundaryResolver.resolve_root_key for backwards compatibility."""
    return BoundaryResolver.resolve_root_key(payload, *candidates)


def _resolve_lock_root_id(payload: dict | None, root_id: str | None) -> str:
    """Resolve DB_KEY for the Lock* function imports.

    Unlike _boundary_root_key, this must NOT filter out the "__CREATE" sentinel:
    lock_control() explicitly special-cases root_key == "__CREATE" (a not-yet-persisted
    draft still needs to answer lock heartbeat/release calls as a benign no-op).
    """
    payload = payload if isinstance(payload, dict) else {}
    for candidate in (root_id, payload.get("DB_KEY"), payload.get("db_key")):
        if candidate:
            raw = str(candidate).strip()
            if raw:
                return raw
    return ""


def _resolve_root_from_payload(payload: dict) -> str:
    root_key = _boundary_root_key(payload)
    if root_key:
        return root_key
    full = payload.get("FullPayload") or payload
    root_block = full.get("root") if isinstance(full, dict) else {}
    return _boundary_root_key(root_block, (root_block or {}).get("DB_KEY"), (root_block or {}).get("db_key"))


def _boundary_parent_key(payload: dict | None = None, *candidates) -> str:
    """Wrapper for BoundaryResolver.resolve_parent_key for backwards compatibility."""
    return BoundaryResolver.resolve_parent_key(payload, *candidates)


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




def _dictionary_config_rows(db: Session) -> list[dict]:
    payload = SettingsService.load_global(SettingsRepo(db))
    raw_required = payload.get("RequiredFieldsJson") or "[]"
    try:
        required_fields = json.loads(raw_required)
    except (json.JSONDecodeError, ValueError, TypeError):
        required_fields = []
    if not isinstance(required_fields, list):
        required_fields = []

    rows = []
    for value in required_fields:
        item = str(value or "").strip()
        if item:
            rows.append({"Domain": "WEB_REQUIRED_FIELD", "Key": item, "Text": item})
    for key, value in (DEFAULT_FRONTEND_VARIABLES or {}).items():
        item_key = str(key or "").strip()
        if item_key:
            rows.append({"Domain": "WEB_VARIABLE", "Key": item_key, "Text": str(value if value is not None else "")})
    return rows






def _attachment_content_base64(entry: AttachmentEntry) -> str:
    path = Path(entry.storage_path or "")
    if not path.exists():
        return ""
    return base64.b64encode(path.read_bytes()).decode("ascii")

def _decode_slug(value: str | None) -> str:
    return unquote(str(value or "").strip())









def _date_ms_from_any(value) -> int:
    """Wrapper for parse_date_ms (backwards compatibility)."""
    return parse_date_ms(value)


def _export_pick_text(value) -> str:
    if value is None:
        return ""
    if isinstance(value, (str, int, float, bool)):
        return str(value).strip()
    if isinstance(value, list):
        for item in value:
            resolved = _export_pick_text(item)
            if resolved:
                return resolved
        return ""
    if isinstance(value, dict):
        for key in ("value", "key", "text", "value1", "low"):
            if key in value and value.get(key) is not None:
                resolved = _export_pick_text(value.get(key))
                if resolved:
                    return resolved
        for key in ("items", "ranges"):
            entries = value.get(key)
            if isinstance(entries, list) and entries:
                resolved = _export_pick_text(entries[0])
                if resolved:
                    return resolved
    return ""


def _normalize_export_limit(value) -> int:
    try:
        parsed = int(str(value or "").strip() or "200000")
    except (ValueError, TypeError):
        parsed = 200000
    return max(1, min(200000, parsed))


def _normalize_export_search_contract(payload: dict | None) -> dict:
    contract = payload if isinstance(payload, dict) else {}
    return {
        "filterId": _export_pick_text(contract.get("filterId")),
        "filterDateFrom": _export_pick_text(contract.get("filterDateFrom")),
        "filterDateTo": _export_pick_text(contract.get("filterDateTo")),
        "filterLocationKey": _export_pick_text(contract.get("filterLocationKey")),
        "filterLpc": _export_pick_text(contract.get("filterLpc")),
        "filterProfession": _export_pick_text(contract.get("filterProfession")),
        "filterStatus": _export_pick_text(contract.get("filterStatus")),
        "searchMode": str(contract.get("searchMode") or "EXACT").strip().upper() or "EXACT",
        "checksSegment": str(contract.get("checksSegment") or "ALL").strip().upper() or "ALL",
        "barriersSegment": str(contract.get("barriersSegment") or "ALL").strip().upper() or "ALL",
    }


def _export_segment_match(segment: str, failed: bool) -> bool:
    normalized = str(segment or "ALL").strip().upper() or "ALL"
    if normalized == "FAILED":
        return bool(failed)
    if normalized == "SUCCESS":
        return not bool(failed)
    return True


def _search_contract_matches(root: ChecklistRoot, contract: dict, db: Session) -> bool:
    filter_id = str(contract.get("filterId") or "").strip().lower()
    filter_date_from = _date_ymd_from_any(contract.get("filterDateFrom"))
    filter_date_to = _date_ymd_from_any(contract.get("filterDateTo")) or filter_date_from
    filter_location_key = str(contract.get("filterLocationKey") or "").strip().lower()
    filter_lpc = str(contract.get("filterLpc") or "").strip().lower()
    filter_profession = str(contract.get("filterProfession") or "").strip().lower()
    filter_status = str(contract.get("filterStatus") or "").strip().upper()
    search_mode = str(contract.get("searchMode") or "EXACT").strip().upper() or "EXACT"
    checks_segment = str(contract.get("checksSegment") or "ALL").strip().upper() or "ALL"
    barriers_segment = str(contract.get("barriersSegment") or "ALL").strip().upper() or "ALL"

    root_id_hex = _hex(root.id)
    root_date = str(root.date or "").strip()
    root_status = _status_external(root.status)
    root_location_key = str(root.location_key or "").strip()
    root_lpc = str(root.lpc or "").strip()
    root_lpc_text = _dict_text(db, "LPC", root_lpc) or root_lpc
    root_profession = str(root.observed_position or "").strip()
    root_profession_text = _dict_text(db, "PROFESSION", root_profession) or root_profession
    checks_failed = any((c.status or "").upper() == "FAIL" for c in (root.checks or []))
    barriers_failed = any(not bool(b.is_active) for b in (root.barriers or []))

    matches = []
    if filter_id:
        matches.append(
            filter_id in str(root.checklist_id or "").lower()
            or filter_id in str(root_id_hex or "").lower()
        )
    if filter_date_from:
        matches.append(bool(root_date) and root_date >= filter_date_from and (not filter_date_to or root_date <= filter_date_to))
    if filter_location_key:
        matches.append(filter_location_key in root_location_key.lower())
    if filter_lpc:
        matches.append(filter_lpc in root_lpc.lower() or filter_lpc in root_lpc_text.lower())
    if filter_profession:
        matches.append(filter_profession in root_profession.lower() or filter_profession in root_profession_text.lower())
    if filter_status:
        matches.append(filter_status == str(root_status or "").upper())
    if checks_segment != "ALL":
        matches.append(_export_segment_match(checks_segment, checks_failed))
    if barriers_segment != "ALL":
        matches.append(_export_segment_match(barriers_segment, barriers_failed))

    if not matches:
        return True
    if search_mode == "LOOSE":
        return any(matches)
    return all(matches)


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


def _attachment_matches_filter(row: AttachmentEntry, filter_expr: str | None) -> bool:
    expr = str(filter_expr or "").strip()
    if not expr:
        return True
    root_match = re.search(r"(?:PARENT_KEY|DB_KEY)\s+eq\s+(?:binary')?([^']+)'", expr, flags=re.IGNORECASE)
    if root_match:
        return _hex(row.root_id) == _normalize_hex_key(root_match.group(1)) or str(row.root_id or "") == _entity_key(root_match.group(1))
    key_match = re.search(r"(?:AttachmentKey|Key)\s+eq\s+'([^']+)'", expr, flags=re.IGNORECASE)
    if key_match:
        return str(row.id or "") == _entity_key(key_match.group(1))
    return True


def _to_attachment(entry: AttachmentEntry, db: Session | None = None) -> dict:
    entry_key = _hex(entry.id)
    category_key = str(entry.category_key or "GEN")
    category_text = category_key
    if db is not None:
        item = db.query(DictionaryItem).filter(DictionaryItem.domain == "ATF_CAT", DictionaryItem.key == category_key).first()
        if item and item.text:
            category_text = str(item.text)
    return ODataSerializer.build_entity(
        "Attachment",
        "AttachmentSet",
        entry_key,
        {
            "AttachmentKey": entry_key,
            "DB_KEY": entry_key,
            "PARENT_KEY": _hex(entry.root_id),
            "FolderKey": str(entry.folder_key or ""),
            "CategoryKey": category_key,
            "CategoryText": category_text,
            "Type": category_key,
            "FileName": str(entry.file_name or ""),
            "Name": str(entry.file_name or ""),
            "MimeType": str(entry.mime_type or "application/octet-stream"),
            "Description": "",
            "FileSize": int(entry.file_size or 0),
            "FileSizeContent": int(entry.file_size or 0),
            "DownloadUrl": f"{SERVICE_ROOT}/AttachmentSet(AttachmentKey='{entry_key}')/$value",
            "DocumentHandle": entry_key,
            "ScanStatus": "OK",
            "ScannedOn": format_datetime(entry.changed_on),
            "CreatedOn": format_datetime(entry.created_on),
            "ChangedOn": format_datetime(entry.changed_on),
        }
    )
def _rate(items, pred) -> float:
    if not items:
        return 100.0
    ok = sum(1 for item in items if pred(item))
    return round((ok / len(items)) * 100, 2)


def _entity_metadata(entity_type: str, entity_set: str, key_value: str) -> dict:
    safe_key = str(key_value or "").replace("'", "''")
    return {
        "type": f"Z_EHS_PRODUCTION_CONTROL_CKLT_SRV.{entity_type}",
        "uri": f"{SERVICE_ROOT}/{entity_set}('{safe_key}')",
    }


def _person_full_name(person: Person) -> str:
    return " ".join([
        str(person.last_name or "").strip(),
        str(person.first_name or "").strip(),
        str(person.middle_name or "").strip()
    ]).strip()


def _to_person_vh(person: Person) -> dict:
    return ODataSerializer.build_entity(
        "PersonVH",
        "PersonVHSet",
        str(person.perner or ""),
        {
            "Pernr": str(person.perner or ""),
            "FirstName": str(person.first_name or ""),
            "LastName": str(person.last_name or ""),
            "MiddleName": str(person.middle_name or ""),
            "Position": str(person.position or ""),
            "OrgUnit": str(person.org_unit or ""),
            "IntegrationName": str(person.integration_name or ""),
            "Begda": date_only_to_odata(person.begda) if getattr(person, "begda", None) else None,
            "Endda": date_only_to_odata(person.endda) if getattr(person, "endda", None) else None,
            "ChangedOn": format_datetime(person.changed_on),
        }
    )


def _to_search(root: ChecklistRoot, db: Session | None = None) -> dict:
    profession = root.observed_position or ""
    lpc = root.lpc or ""
    root_key = _hex(root.id)
    profession_text = _dict_text(db, "PROFESSION", profession) if db else ""
    lpc_text = _dict_text(db, "LPC", lpc) if db else ""
    return ODataSerializer.build_entity(
        "ChecklistSearch",
        "ChecklistSearchSet",
        root_key,
        {
            "DB_KEY": root_key,
            "Key": root_key,
            "Id": root.checklist_id or "",
            "DateCheck": date_only_to_odata(_datecheck_datetime(root).date()),
            "TimeCheck": root.time_check or "",
            "TimeZone": root.time_zone or "",
            "LocationKey": root.location_key or "",
            "Bukrs": root.bukrs or "",
            "Lpc": lpc,
            "LpcText": lpc_text or lpc,
            "Profession": profession,
            "ProfessionText": profession_text or profession,
            "EquipName": root.equipment or "",
            "Status": _status_external(root.status),
            "IntegrationFlag": bool(root.integration_flag),
            "SourceKey": "INTEGRATION" if bool(root.integration_flag) else "WEB",
            "ChangedOn": format_datetime(root.changed_on),
            "CreatedOn": format_datetime(root.created_on),
            "RequestId": root.checklist_id or "",
            "HasFailedChecks": any((c.status or "").upper() == "FAIL" for c in root.checks),
            "HasFailedBarriers": any(not bool(b.is_active) for b in root.barriers),
            "SuccessChecksRate": _rate(root.checks, lambda c: (c.status or "").upper() != "FAIL"),
            "SuccessBarriersRate": _rate(root.barriers, lambda b: bool(b.is_active)),
            "ChecksTotal": len(root.checks or []),
            "BarriersTotal": len(root.barriers or []),
        }
    )


def _to_root(root: ChecklistRoot, db: Session | None = None) -> dict:
    s = _to_search(root, db=db)
    return ODataSerializer.build_entity(
        "ChecklistRoot",
        "ChecklistRootSet",
        s["DB_KEY"],
        {
            "DB_KEY": s["DB_KEY"],
            "Key": s["Key"],
            "RequestId": s["RequestId"],
            "Id": s["Id"],
            "ChangedOn": s["ChangedOn"],
            "CreatedOn": s["CreatedOn"],
            "VersionNumber": int(root.version_number or 0),
            "Status": s["Status"],
            "HasFailedChecks": s["HasFailedChecks"],
            "HasFailedBarriers": s["HasFailedBarriers"],
            "SuccessChecksRate": s["SuccessChecksRate"],
            "SuccessBarriersRate": s["SuccessBarriersRate"],
            "ChecksTotal": s["ChecksTotal"],
            "BarriersTotal": s["BarriersTotal"],
        }
    )


def _to_basic(root: ChecklistRoot) -> dict:
    root_key = _hex(root.id)
    return ODataSerializer.build_entity(
        "ChecklistBasicInfo",
        "ChecklistBasicInfoSet",
        root_key,
        {
            "DB_KEY": root_key,
            "ChecklistId": root.checklist_id or "",
            "LocationKey": root.location_key or "",
            "LocationName": root.location_name or root.location_text or "",
            "LocationText": root.location_text or root.location_name or "",
            "Bukrs": root.bukrs or "",
            "ObserverPernr": root.observer_perner or "",
            "ObserverName": root.observer_fullname or "",
            "ObserverFullname": root.observer_fullname or "",
            "ObserverPosition": root.observer_position or "",
            "ObserverOrgUnit": root.observer_orgunit or "",
            "ObservedPernr": root.observed_perner or "",
            "ObservedName": root.observed_fullname or "",
            "ObservedFullname": root.observed_fullname or "",
            "ObservedPosition": root.observed_position or "",
            "ObservedOrgUnit": root.observed_orgunit or "",
            "Lpc": root.lpc or "",
            "LpcText": root.lpc_text or root.lpc or "",
            "Profession": root.observed_position or "",
            "ProfessionText": root.observed_position or "",
            "ChecksNumber": "",
            "ChecksNumberText": "",
            "BarriersNumber": "",
            "BarriersNumberText": "",
            "DateCheck": date_only_to_odata(_datecheck_datetime(root).date()),
            "TimeCheck": root.time_check or "",
            "TimeZone": root.time_zone or "",
            "EquipName": root.equipment or "",
        }
    )


def _to_permission(root: ChecklistRoot, user_id: str | None, db: Session | None = None) -> dict:
    permission = AuthorizationService.checklist_permissions(root, user_id, db=db)
    root_key = _hex(root.id)
    return ODataSerializer.build_entity(
        "ChecklistPermission",
        "ChecklistPermissionSet",
        root_key,
        {
            "DB_KEY": root_key,
            "UserId": str(permission.get("user_id") or ""),
            "AuthObject": str(permission.get("auth_object") or ""),
            "CreateOperation": str(permission.get("create_operation") or ""),
            "ViewOperation": str(permission.get("view_operation") or ""),
            "ChangeOperation": str(permission.get("change_operation") or ""),
            "DeleteOperation": str(permission.get("delete_operation") or ""),
            "GrantedOperations": ",".join(permission.get("granted_operations") or []),
            "CanCreate": bool(permission.get("can_create")),
            "CanView": bool(permission.get("can_view")),
            "CanEdit": bool(permission.get("can_edit")),
            "CanDelete": bool(permission.get("can_delete")),
            "ReasonCode": str(permission.get("reason_code") or ""),
            "Message": str(permission.get("message") or ""),
        }
    )


def _to_create_permission(user_id: str | None, db: Session | None = None) -> dict:
    permission = AuthorizationService.create_permission(user_id, db=db)
    root_key = "CURRENT"
    return ODataSerializer.build_entity(
        "ChecklistPermission",
        "ChecklistCreatePermissionSet",
        root_key,
        {
            "DB_KEY": root_key,
            "UserId": str(permission.get("user_id") or ""),
            "AuthObject": str(permission.get("auth_object") or ""),
            "CreateOperation": str(permission.get("create_operation") or ""),
            "ViewOperation": str(permission.get("view_operation") or ""),
            "ChangeOperation": str(permission.get("change_operation") or ""),
            "DeleteOperation": str(permission.get("delete_operation") or ""),
            "GrantedOperations": ",".join(permission.get("granted_operations") or []),
            "CanCreate": bool(permission.get("can_create")),
            "CanView": bool(permission.get("can_view")),
            "CanEdit": bool(permission.get("can_edit")),
            "CanDelete": bool(permission.get("can_delete")),
            "ReasonCode": str(permission.get("reason_code") or ""),
            "Message": str(permission.get("message") or ""),
        }
    )




def _permission_groups(profile: dict) -> list[dict]:
    groups: dict[str, dict] = {}
    order = {"01": 1, "02": 2, "03": 3, "06": 4}
    for permission in profile.get("permissions") or []:
        code = str((permission or {}).get("code") or "").strip()
        if not code:
            continue
        scope_kind = str((permission or {}).get("scope_kind") or "all").strip().lower() or "all"
        scope_value = str((permission or {}).get("scope_value") or "ALL").strip() or "ALL"
        group_key = f"{scope_kind}:{scope_value.upper()}"
        group = groups.setdefault(group_key, {
            "scopeKind": scope_kind,
            "scopeValue": scope_value.upper() if scope_kind == "bukrs" else "ALL",
            "permissionCodes": [],
        })
        if code not in group["permissionCodes"]:
            group["permissionCodes"].append(code)
    result = list(groups.values())
    for item in result:
        item["permissionCodes"] = sorted(item.get("permissionCodes") or [], key=lambda value: order.get(value, 99))
    result.sort(key=lambda item: (
        0 if str(item.get("scopeKind") or "all") == "all" else 1,
        str(item.get("scopeValue") or "ALL")
    ))
    return result


def _current_user_summary(profile: dict) -> str:
    groups = _permission_groups(profile)
    if not groups:
        return "Permissions: none"
    parts = []
    for item in groups:
        scope_kind = str(item.get("scopeKind") or "all").strip().lower() or "all"
        scope_value = str(item.get("scopeValue") or "ALL").strip() or "ALL"
        prefix = "ALL" if scope_kind == "all" or scope_value == "ALL" else f"BUKRS {scope_value}"
        parts.append(prefix + ": " + ",".join(item.get("permissionCodes") or []))
    return "Permissions: " + "; ".join(parts)


def _to_current_user(profile: dict) -> dict:
    permissions = list(profile.get("permissions") or [])
    permission_codes = [str((permission or {}).get("code") or "") for permission in permissions]
    key = "CURRENT"
    return ODataSerializer.build_entity(
        "CurrentUser",
        "CurrentUserSet",
        key,
        {
            "Key": key,
            "FullName": str(profile.get("full_name") or ""),
            "PermissionsCsv": ",".join(permission_codes),
            "PermissionRulesJson": json.dumps(permissions, ensure_ascii=False),
            "CanView": "03" in permission_codes,
            "CanEdit": "02" in permission_codes,
            "CanDelete": "06" in permission_codes,
            "SummaryText": _current_user_summary(profile),
        }
    )


def _to_check(item: ChecklistCheck) -> dict:
    key = _hex(item.id)
    return ODataSerializer.build_entity(
        "ChecklistCheck",
        "ChecklistCheckSet",
        key,
        {
            "DB_KEY": key,
            "PARENT_KEY": _hex(item.root_id),
            "ChecksNum": int(item.position or 0),
            "Text": item.text or "",
            "Comment": item.comment or "",
            "Result": (item.status or "").upper() != "FAIL",
            "ChangedOn": format_datetime(item.changed_on),
        }
    )


def _to_barrier(item: ChecklistBarrier) -> dict:
    key = _hex(item.id)
    return ODataSerializer.build_entity(
        "ChecklistBarrier",
        "ChecklistBarrierSet",
        key,
        {
            "DB_KEY": key,
            "PARENT_KEY": _hex(item.root_id),
            "BarriersNum": int(item.position or 0),
            "Text": item.description or "",
            "Comment": item.comment or "",
            "Result": bool(item.is_active),
            "ChangedOn": format_datetime(item.changed_on),
        }
    )


_CHECK_KIND = _DetailKind(
    model=ChecklistCheck,
    entity_tag="CHECK",
    not_found_message="Check row not found",
    field_map=CHECK_MAP,
    key_aliases=("check_uuid", "Key", "key"),
    text_aliases=("text", "Text"),
    number_aliases=("checks_num", "ChecksNum", "position", "Position"),
    number_field="ChecksNum",
    to_kwargs=_check_kwargs,
    apply_update=_apply_check_update,
    serialize=_to_check,
)
_BARRIER_KIND = _DetailKind(
    model=ChecklistBarrier,
    entity_tag="BARRIER",
    not_found_message="Barrier row not found",
    field_map=BARRIER_MAP,
    key_aliases=("barrier_uuid", "Key", "key"),
    text_aliases=("text", "Text", "description", "Description"),
    number_aliases=("barriers_num", "BarriersNum", "position", "Position"),
    number_field="BarriersNum",
    to_kwargs=_barrier_kwargs,
    apply_update=_apply_barrier_update,
    serialize=_to_barrier,
)


def _agg_changed_on(root: ChecklistRoot):
    points = [root.changed_on, root.created_on]
    points.extend([c.changed_on for c in (root.checks or []) if c.changed_on])
    points.extend([b.changed_on for b in (root.barriers or []) if b.changed_on])
    points = [p for p in points if p is not None]
    return max(points) if points else now_utc()


def _is_expired_lock(expires_on: datetime | None) -> bool:
    if not expires_on:
        return False
    dt = expires_on if expires_on.tzinfo else expires_on.replace(tzinfo=timezone.utc)
    return dt < now_utc()


def _build_lock_status_row(db: Session, root_uuid: str, session_guid: str = "") -> dict:
    root_key_hex = _hex(root_uuid)
    meta = _entity_metadata("LockStatus", "LockStatusSet", root_key_hex)

    def _row(ok: bool, reason: str, owner: str, expires_at) -> dict:
        exp = format_datetime(expires_at)
        return {
            "__metadata": meta,
            "DB_KEY": root_key_hex,
            "Ok": ok,
            "Success": ok,
            "ReasonCode": reason,
            "Owner": owner,
            "ExpiresOn": exp,
            "LockExpires": exp,
        }

    if root_uuid == "__CREATE":
        return _row(bool(session_guid), "OWNED_BY_YOU" if session_guid else "FREE", CurrentUserService.resolve_uname(db=db) if session_guid else "", None)

    active = db.query(LockEntry).filter(LockEntry.db_key == root_uuid, LockEntry.is_killed.is_(False)).first()
    if not active:
        own = db.query(LockEntry).filter(
            LockEntry.db_key == root_uuid,
            LockEntry.session_guid == session_guid,
        ).order_by(LockEntry.last_refresh_at.desc()).first() if session_guid else None
        if own:
            if own.is_killed:
                return _row(False, "KILLED", own.user_id or "", own.expires_at)
            if _is_expired_lock(own.expires_at):
                own.is_killed = True
                db.commit()
                return _row(False, "EXPIRED", own.user_id or "", own.expires_at)
        return _row(True, "FREE", "", None)
    if _is_expired_lock(active.expires_at):
        active.is_killed = True
        db.commit()
        return _row(False, "EXPIRED", active.user_id or "", active.expires_at)
    if active.session_guid == session_guid and session_guid:
        return _row(True, "OWNED_BY_YOU", active.user_id, active.expires_at)
    return _row(False, "LOCKED_BY_OTHER", active.user_id, active.expires_at)


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
    selected = {k: v for k, v in row.items() if k in keep}
    if "__metadata" in row:
        selected["__metadata"] = row["__metadata"]
    return selected


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




def _if_match_check(if_match: str | None, agg: datetime, version_number: int | None = None):
    if not validate_if_match(if_match, etag_for_datetime(agg, version_number)):
        raise ValueError("PRECONDITION_FAILED")


def _optimistic_check(client_agg: str | None, root: ChecklistRoot):
    if not client_agg:
        raise ValueError("VALIDATION_ERROR")
    cms = int(_parse_odata_datetime(client_agg).timestamp() * 1000)
    current_dt = _agg_changed_on(root)
    current_dt = current_dt if current_dt.tzinfo else current_dt.replace(tzinfo=timezone.utc)
    current = int(current_dt.timestamp() * 1000)
    if cms != current:
        raise ValueError("CONFLICT")


def _load_root_or_error(db: Session, root_key: str):
    try:
        _entity_key(root_key)
    except ValueError:
        return None, _err(400, "VALIDATION_ERROR", "DB_KEY must be RAW16 HEX (32 chars)")
    root = db.query(ChecklistRoot).filter(
        ChecklistRoot.id == _entity_key(root_key),
        ChecklistRoot.is_deleted.isnot(True),
    ).first()
    if not root:
        return None, _err(404, "NOT_FOUND", "Checklist not found")
    return root, None


def _parse_client_version(value) -> int:
    try:
        return int(str(value).strip())
    except (ValueError, TypeError):
        return 0


def _check_client_version(payload: dict, root: ChecklistRoot):
    client_version = _parse_client_version(payload.get("client_version") or payload.get("ClientVersion"))
    if client_version <= 0:
        raise ValueError("VALIDATION_ERROR")
    if int(root.version_number or 0) != client_version:
        raise ValueError("CONFLICT")
    return client_version


def _save_request_root(payload: dict | None) -> dict:
    body = payload or {}
    if isinstance(body.get("Payload"), dict):
        body = body.get("Payload")
    root = body.get("root")
    return root if isinstance(root, dict) else {}


def _save_request_root_key(payload: dict | None) -> str:
    body = payload or {}
    if isinstance(body.get("Payload"), dict):
        body = body.get("Payload")
    root = _save_request_root(payload)
    return _boundary_root_key(
        root,
        root.get("db_key"),
        body.get("DB_KEY"),
        body.get("db_key"),
    )


def _save_request_session(payload: dict | None) -> str:
    body = payload or {}
    if isinstance(body.get("Payload"), dict):
        body = body.get("Payload")
    root = _save_request_root(body)
    return str(
        body.get("SessionGuid")
        or body.get("session_guid")
        or root.get("SessionGuid")
        or root.get("session_guid")
        or ""
    ).strip()


def _request_uuid_or_new(value: str | None) -> str:
    raw = str(value or "").strip()
    if not raw:
        return str(uuid.uuid4())
    try:
        _normalize_hex_key(raw)
        return _entity_key(raw)
    except ValueError:
        return str(uuid.uuid4())


def _apply_save_root(root: ChecklistRoot, root_payload: dict | None, db: Session) -> None:
    data = root_payload if isinstance(root_payload, dict) else {}
    status = _pick_text(data, "status", "Status")
    request_id = _pick_text(data, "checklist_id", "RequestId", "Id")
    lpc = _pick_text(data, "lpc", "Lpc")
    profession = _pick_text(data, "observed_position", "profession", "Profession")

    if status:
        root.status = _normalize_status_input(status)
    if request_id:
        root.checklist_id = request_id
    if lpc:
        root.lpc = lpc
        root.lpc_text = _dict_text(db, "LPC", lpc) if lpc else root.lpc_text
    if profession:
        root.observed_position = profession

    if _pick_first_present(data, "date", "DateCheck") is not None:
        root.date = _date_ymd_from_any(_pick_first_present(data, "date", "DateCheck"))
    if _pick_first_present(data, "time_check", "time", "TimeCheck") is not None:
        root.time_check = _pick_text(data, "time_check", "time", "TimeCheck")
    if _pick_first_present(data, "time_zone", "timezone", "TimeZone") is not None:
        root.time_zone = _pick_text(data, "time_zone", "timezone", "TimeZone")
    if _pick_first_present(data, "equipment", "EquipName") is not None:
        root.equipment = _pick_text(data, "equipment", "EquipName")

    if _pick_first_present(data, "location_key", "LocationKey") is not None:
        root.location_key = _pick_text(data, "location_key", "LocationKey")
    if _pick_first_present(data, "location_name", "LocationName") is not None:
        root.location_name = _pick_text(data, "location_name", "LocationName")
    if _pick_first_present(data, "location_text", "LocationText") is not None:
        root.location_text = _pick_text(data, "location_text", "LocationText")

    if _pick_first_present(data, "observer_fullname", "ObserverFullname") is not None:
        root.observer_fullname = _pick_text(data, "observer_fullname", "ObserverFullname")
    if _pick_first_present(data, "observer_perner", "ObserverPernr") is not None:
        root.observer_perner = _pick_text(data, "observer_perner", "ObserverPernr")
    if _pick_first_present(data, "observer_position", "ObserverPosition") is not None:
        root.observer_position = _pick_text(data, "observer_position", "ObserverPosition")
    if _pick_first_present(data, "observer_orgunit", "ObserverOrgUnit") is not None:
        root.observer_orgunit = _pick_text(data, "observer_orgunit", "ObserverOrgUnit")

    if _pick_first_present(data, "observed_fullname", "ObservedFullname") is not None:
        root.observed_fullname = _pick_text(data, "observed_fullname", "ObservedFullname")
    if _pick_first_present(data, "observed_perner", "ObservedPernr") is not None:
        root.observed_perner = _pick_text(data, "observed_perner", "ObservedPernr")
    if _pick_first_present(data, "observed_position", "profession", "Profession", "ObservedPosition") is not None:
        root.observed_position = _pick_text(data, "observed_position", "profession", "Profession", "ObservedPosition")
    if _pick_first_present(data, "observed_orgunit", "ObservedOrgUnit") is not None:
        root.observed_orgunit = _pick_text(data, "observed_orgunit", "ObservedOrgUnit")


def _normalize_detail_row_for_create(row: dict, kind: _DetailKind) -> dict:
    return {
        "text": _pick_text(row, *kind.text_aliases),
        "comment": _pick_text(row, "comment", "Comment"),
        "result": _pick_bool(row, "result", "Result", default=True),
        "position": _coerce_int(_pick_first_present(row, *kind.number_aliases), 0),
    }


def _normalize_detail_row_for_update(row: dict, kind: _DetailKind) -> dict:
    normalized = {}
    if _pick_first_present(row, *kind.text_aliases) is not None:
        normalized["text"] = _pick_text(row, *kind.text_aliases)
    if _pick_first_present(row, "comment", "Comment") is not None:
        normalized["comment"] = _pick_text(row, "comment", "Comment")
    if _pick_first_present(row, "result", "Result") is not None:
        normalized["result"] = _pick_bool(row, "result", "Result", default=True)
    if _pick_first_present(row, *kind.number_aliases) is not None:
        normalized["position"] = _coerce_int(_pick_first_present(row, *kind.number_aliases), 0)
    return normalized


def _apply_save_detail_rows(db: Session, root: ChecklistRoot, items, kind: _DetailKind) -> None:
    """Shared upsert-by-list body for SaveChanges/AutoSave, parameterized over Check vs Barrier
    (see _DetailKind for what actually differs between them)."""
    for item in items if isinstance(items, list) else []:
        row = item if isinstance(item, dict) else {}
        mode = str(row.get("edit_mode") or row.get("EditMode") or "U").upper()
        raw_key = str(_pick_first_present(row, *kind.key_aliases) or "").strip()
        key = _entity_key(raw_key) if raw_key else ""
        if mode == "C":
            created = kind.model(
                id=str(uuid.uuid4()),
                root_id=root.id,
                changed_on=now_utc(),
                **kind.to_kwargs(_normalize_detail_row_for_create(row, kind)),
            )
            db.add(created)
        elif mode == "U" and key:
            current = db.query(kind.model).filter(kind.model.id == key, kind.model.root_id == root.id).first()
            if current:
                kind.apply_update(current, _normalize_detail_row_for_update(row, kind))
                current.changed_on = now_utc()
        elif mode == "D" and key:
            db.query(kind.model).filter(kind.model.id == key, kind.model.root_id == root.id).delete()


def _normalize_change_fields_for_create(fields: dict, kind: _DetailKind) -> dict:
    return {
        "text": fields.get("Text", ""),
        "comment": fields.get("Comment", ""),
        "result": fields.get("Result", True),
        "position": int(fields.get(kind.number_field, 0)),
    }


def _normalize_change_fields_for_update(fields: dict) -> dict:
    normalized = {}
    if "Text" in fields:
        normalized["text"] = fields["Text"]
    if "Comment" in fields:
        normalized["comment"] = fields["Comment"]
    if "Result" in fields:
        normalized["result"] = fields["Result"]
    return normalized


def _apply_change_detail(db: Session, root: ChecklistRoot, kind: _DetailKind, mode: str, key: str, fields: dict, change: dict, row_changed_on) -> dict | None:
    """Shared body for the legacy _apply_change single-record import, parameterized
    over Check vs Barrier (see _DetailKind)."""
    if mode == "C":
        created = kind.model(
            id=str(uuid.uuid4()),
            root_id=root.id,
            changed_on=row_changed_on,
            **kind.to_kwargs(_normalize_change_fields_for_create(fields, kind)),
        )
        db.add(created)
        return {"Entity": kind.entity_tag, "ClientKey": change.get("Key") or "", "ServerKey": _hex(created.id)}
    if mode == "U":
        row = db.query(kind.model).filter(kind.model.id == key).first()
        if row:
            kind.apply_update(row, _normalize_change_fields_for_update(fields))
            row.changed_on = row_changed_on
        return None
    if mode == "D":
        db.query(kind.model).filter(kind.model.id == key).delete()
        return None
    return None


def _apply_change(db: Session, root: ChecklistRoot, change: dict):
    entity = str(change.get("Entity") or "").upper()
    mode = str(change.get("EditMode") or "U").upper()
    fields = change.get("Fields") or {}
    key = _entity_key(str(change.get("Key") or ""))
    updated_mapping = None
    row_changed_on = now_utc()

    if entity == "ROOT" and mode == "U":
        _apply_root_payload(root, fields)
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
        if fields.get("TimeCheck") is not None:
            root.time_check = str(fields.get("TimeCheck") or "")
        if fields.get("TimeZone") is not None:
            root.time_zone = str(fields.get("TimeZone") or "")
        root.changed_on = row_changed_on
    elif entity == "CHECK":
        updated_mapping = _apply_change_detail(db, root, _CHECK_KIND, mode, key, fields, change, row_changed_on)
    elif entity == "BARRIER":
        updated_mapping = _apply_change_detail(db, root, _BARRIER_KIND, mode, key, fields, change, row_changed_on)

    return updated_mapping


def _validate_status_change(root: ChecklistRoot, new_status: str):
    old_status = _status_external(root.status)
    if old_status == new_status:
        return
    allowed = _STATUS_TRANSITIONS.get(old_status)
    if allowed is not None and new_status not in allowed:
        raise ValueError("VALIDATION_ERROR")


def _import_payload(root_id: str | None = None, session_guid: str | None = None, status: str | None = None, payload: dict | None = None) -> dict:
    body = payload or {}
    # root_id is already fully resolved by the caller (may legitimately be the "__CREATE"
    # sentinel for lock imports) - do not re-run it through _boundary_root_key, which would
    # filter "__CREATE" back out to "".
    result = {
        "DB_KEY": str(root_id or "").strip() or _boundary_root_key(body),
        "SessionGuid": body.get("SessionGuid") or body.get("session_guid") or session_guid,
        "Status": body.get("Status") or body.get("status") or status,
        "Changes": body.get("Changes") or [],
        "ClientAggChangedOn": body.get("ClientAggChangedOn"),
        "FullPayload": body.get("FullPayload") or body,
    }
    # Preserve lock-specific fields that must not be dropped
    for _k in ("Force", "StealFrom", "NewStatus"):
        if _k in body:
            result[_k] = body[_k]
    return result


def _normalize_root_filter_aliases(filter_value: str | None) -> str:
    return re.sub(r"\bRootKey\b|\bRootId\b", "PARENT_KEY", str(filter_value or ""), flags=re.IGNORECASE)



def _merge_query_and_payload(request: Request, payload: dict | None = None) -> dict:
    merged = dict(request.query_params)
    merged.update(payload or {})
    return merged


def _lock_import_payload(action: str, root_id: str, session_guid: str, payload: dict | None) -> dict:
    req = _import_payload(root_id=root_id, session_guid=session_guid, payload=payload)
    req["Action"] = action
    return req


@router.api_route(f"{SERVICE_ROOT}/", methods=["GET", "HEAD"])
def service_document():
    return odata_entity({"EntitySets": [
        "ChecklistSearchSet", "ChecklistRootSet", "ChecklistBasicInfoSet", "ChecklistCheckSet", "ChecklistBarrierSet",
        "DictionaryItemSet", "PersonVHSet", "LastChangeSet", "LockStatusSet", "ChecklistPermissionSet", "ChecklistCreatePermissionSet", "RuntimeSettingsSet",
        "CurrentUserSet", "SimpleAnalyticalSet", "WorkflowAnalyticsBreakdownSet", "AnalyticsRefreshStateSet", "AttachmentFolderSet", "AttachmentSet",
    ]})


@router.get(f"{SERVICE_ROOT}/$metadata")
def metadata():
    oResponse = Response(content=get_metadata(), media_type="application/xml; charset=utf-8")
    sRefreshedAt = metadata_refreshed_at_iso()
    if sRefreshedAt:
        oResponse.headers["X-Metadata-Refreshed-At"] = sRefreshedAt
    return oResponse


@router.get(f"{SERVICE_ROOT}/ChecklistSearchSet")
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
    db_filter = filter
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True))
    expr = FilterParser.parse(ChecklistRoot, db_filter, field_map=SEARCH_MAP)
    if expr is not None:
        query = query.filter(expr)
    roots = query.all()
    mapped = [_to_search(r, db=db) for r in roots]
    mapped = _apply_orderby_rows(mapped, orderby)
    top, skip = parse_paging(top, skip, DEFAULT_PAGE_SIZE)
    total = len(mapped)
    paged = mapped[skip: skip + top]
    return odata_collection([_apply_select(r, select) for r in paged], with_inlinecount(inlinecount, total))


@router.get(f"{SERVICE_ROOT}/ChecklistSearchSet/$count")
def checklist_search_count(filter: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    db_filter = filter
    query = db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True))
    expr = FilterParser.parse(ChecklistRoot, db_filter, field_map=SEARCH_MAP)
    if expr is not None:
        query = query.filter(expr)
    return Response(content=str(query.count()), media_type="text/plain")


@router.get(f"{SERVICE_ROOT}/ChecklistRootSet")
def checklist_root_set(filter: str | None = Query(None, alias="$filter"), orderby: str | None = Query(None, alias="$orderby"), top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), select: str | None = Query(None, alias="$select"), db: Session = Depends(get_db)):
    rows, total = _apply_order_filter(db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)), ChecklistRoot, ROOT_MAP, filter, orderby, top, skip)
    return odata_payload([_apply_select(_to_root(r, db=db), select) for r in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistRootSet({{entity_key}})")
def checklist_root_entity(entity_key: str, response: Response, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    response.headers["ETag"] = format_entity_etag(_agg_changed_on(root), root.version_number)
    return odata_entity(_to_root(root, db=db))


@router.post(f"{SERVICE_ROOT}/ChecklistRootSet")
def checklist_root_create(payload: dict, response: Response, db: Session = Depends(get_db)):
    basic_values = _normalize_basic_payload(payload, db)
    requested_id = _pick_text(payload, "RequestId", "Id", "checklist_id")
    requested_status = _pick_text(payload, "Status", "status") or "DRAFT"
    user_name = CurrentUserService.resolve_uname(db=db)

    root = ChecklistRoot(
        id=str(uuid.uuid4()),
        checklist_id=requested_id or _next_checklist_id(db),
        lpc=str(payload.get("lpc") or basic_values.get("lpc") or ""),
        status=_normalize_status_input(requested_status),
        integration_flag=False,
        created_by=user_name,
        changed_by=user_name,
        version_number=1,
    )
    _apply_basic_payload(root, basic_values)
    root.changed_on = now_utc()
    db.add(root)
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    response.headers["sap-message"] = build_sap_message("Checklist created", "success", code="CREATED")
    return odata_entity(_to_root(root, db=db))


@router.patch(f"{SERVICE_ROOT}/ChecklistRootSet({{entity_key}})")
def checklist_root_update(entity_key: str, payload: dict, response: Response, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    session_guid = str(payload.get("SessionGuid") or payload.get("session_guid") or "").strip()
    if session_guid:
        try:
            LockService.validate_session_lock(db, root.id, session_guid)
        except ValueError as ex:
            if str(ex) in {"LOCK_MISSING", "LOCK_NOT_OWNED_BY_SESSION"}:
                return _err(409, str(ex), "Active lock for session is required")
            return _err(409, "LOCK_EXPIRED", "Lock expired")

    _apply_root_payload(root, payload)
    _apply_basic_payload(root, _normalize_basic_payload(payload, db))
    root.changed_by = CurrentUserService.resolve_uname(db=db) or root.changed_by or "ANON"
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    response.headers["sap-message"] = build_sap_message("Checklist updated", "success", code="SAVED")
    return odata_entity(_to_root(root, db=db))


@router.delete(f"{SERVICE_ROOT}/ChecklistRootSet({{entity_key}})")
def checklist_root_delete(entity_key: str, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    root.is_deleted = True
    root.changed_on = now_utc()
    db.commit()
    AnalyticsService.mark_dirty()
    return Response(status_code=204)


@router.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet")
def checklist_basic_info_set(filter: str | None = Query(None, alias="$filter"), top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    rows, total = _apply_order_filter(db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)), ChecklistRoot, {"DB_KEY": "id"}, filter, None, top, skip)
    return odata_payload([_to_basic(r) for r in rows], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistBasicInfoSet({{entity_key}})")
def checklist_basic_info_entity(entity_key: str, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    return odata_entity(_to_basic(root))


def _normalize_rest_payload_for_create(payload: dict, kind: _DetailKind) -> dict:
    return {
        "text": str(payload.get("Text") or "").strip(),
        "comment": str(payload.get("Comment") or "").strip(),
        "result": bool(payload.get("Result")),
        "position": int(payload.get(kind.number_field) or 0) or 1,
    }


def _normalize_rest_payload_for_update(payload: dict, kind: _DetailKind) -> dict:
    # Result is always overwritten (falls back to False, not left untouched) to match
    # the pre-existing REST PATCH contract: a PATCH without "Result" clears it to FAIL/inactive.
    normalized = {"result": bool(payload.get("Result"))}
    if payload.get("Text") is not None:
        normalized["text"] = str(payload.get("Text") or "").strip()
    if payload.get("Comment") is not None:
        normalized["comment"] = str(payload.get("Comment") or "").strip()
    if payload.get(kind.number_field) is not None:
        normalized["position"] = int(payload.get(kind.number_field) or 0) or 1
    return normalized


def _detail_list(db: Session, kind: _DetailKind, filter_expr: str | None, top: int, skip: int, inlinecount: str | None):
    """Shared GET-collection body for ChecklistCheckSet/ChecklistBarrierSet."""
    filter_expr = re.sub(r"\bRootId\b", "PARENT_KEY", str(filter_expr or ""), flags=re.IGNORECASE)
    filter_expr = re.sub(r"\bRootKey\b", "PARENT_KEY", filter_expr, flags=re.IGNORECASE)
    rows, total = _apply_order_filter(db.query(kind.model), kind.model, kind.field_map, filter_expr, None, top, skip)
    return odata_payload([kind.serialize(r) for r in rows], total if inlinecount == "allpages" else None)


def _detail_create(db: Session, payload: dict, kind: _DetailKind):
    """Shared POST body for ChecklistCheckSet/ChecklistBarrierSet."""
    root, err = _load_root_or_error(db, _boundary_parent_key(payload))
    if err:
        return err
    row = kind.model(
        id=str(uuid.uuid4()),
        root_id=root.id,
        created_on=now_utc(),
        changed_on=now_utc(),
        **kind.to_kwargs(_normalize_rest_payload_for_create(payload, kind)),
    )
    db.add(row)
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    return odata_entity(kind.serialize(row))


def _detail_update(db: Session, entity_key: str, payload: dict, kind: _DetailKind):
    """Shared PATCH body for ChecklistCheckSet({key})/ChecklistBarrierSet({key})."""
    key = _entity_key(entity_key)
    row = db.query(kind.model).filter(kind.model.id == key).first()
    if not row:
        return _err(404, "NOT_FOUND", kind.not_found_message)
    kind.apply_update(row, _normalize_rest_payload_for_update(payload, kind))
    row.changed_on = now_utc()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == row.root_id).first()
    if root:
        root.changed_on = now_utc()
        root.version_number = int(root.version_number or 0) + 1
    db.commit()
    return odata_entity(kind.serialize(row))


def _detail_delete(db: Session, entity_key: str, kind: _DetailKind):
    """Shared DELETE body for ChecklistCheckSet({key})/ChecklistBarrierSet({key})."""
    key = _entity_key(entity_key)
    row = db.query(kind.model).filter(kind.model.id == key).first()
    if not row:
        return _err(404, "NOT_FOUND", kind.not_found_message)
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == row.root_id).first()
    db.delete(row)
    if root:
        root.changed_on = now_utc()
        root.version_number = int(root.version_number or 0) + 1
    db.commit()
    return Response(status_code=204)


@router.get(f"{SERVICE_ROOT}/ChecklistCheckSet")
def checklist_check_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), top: int = Query(20, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    return _detail_list(db, _CHECK_KIND, filter, top, skip, inlinecount)


@router.post(f"{SERVICE_ROOT}/ChecklistCheckSet")
def checklist_check_create(payload: dict, db: Session = Depends(get_db)):
    return _detail_create(db, payload, _CHECK_KIND)


@router.patch(f"{SERVICE_ROOT}/ChecklistCheckSet({{entity_key}})")
def checklist_check_update(entity_key: str, payload: dict, db: Session = Depends(get_db)):
    return _detail_update(db, entity_key, payload, _CHECK_KIND)


@router.delete(f"{SERVICE_ROOT}/ChecklistCheckSet({{entity_key}})")
def checklist_check_delete(entity_key: str, db: Session = Depends(get_db)):
    return _detail_delete(db, entity_key, _CHECK_KIND)


@router.get(f"{SERVICE_ROOT}/ChecklistBarrierSet")
def checklist_barrier_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), top: int = Query(20, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    return _detail_list(db, _BARRIER_KIND, filter, top, skip, inlinecount)


@router.post(f"{SERVICE_ROOT}/ChecklistBarrierSet")
def checklist_barrier_create(payload: dict, db: Session = Depends(get_db)):
    return _detail_create(db, payload, _BARRIER_KIND)


@router.patch(f"{SERVICE_ROOT}/ChecklistBarrierSet({{entity_key}})")
def checklist_barrier_update(entity_key: str, payload: dict, db: Session = Depends(get_db)):
    return _detail_update(db, entity_key, payload, _BARRIER_KIND)


@router.delete(f"{SERVICE_ROOT}/ChecklistBarrierSet({{entity_key}})")
def checklist_barrier_delete(entity_key: str, db: Session = Depends(get_db)):
    return _detail_delete(db, entity_key, _BARRIER_KIND)


@router.get(f"{SERVICE_ROOT}/DictionaryItemSet")
def dictionary_item_set(filter: str | None = Query(None, alias="$filter"), orderby: str | None = Query(None, alias="$orderby"), top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"), skip: int = Query(0, alias="$skip"), inlinecount: str | None = Query(None, alias="$inlinecount"), db: Session = Depends(get_db)):
    def _to_dict_row(domain: str, key: str, text: str) -> dict:
        composite_key = f"{domain}|{key}"
        return {
            "__metadata": _entity_metadata("DictionaryItem", "DictionaryItemSet", composite_key),
            "Domain": domain,
            "Key": key,
            "Text": text,
        }

    # Push $filter down to SQL for the DB-backed rows (this table can hold hundreds/thousands
    # of reference codes) - only the small, config-derived synthetic rows below (bounded by
    # config size, never grows with real data) are still filtered in Python, since they don't
    # exist as DB rows at all and can't be pushed down.
    query = db.query(DictionaryItem)
    if filter:
        try:
            expr = FilterParser.parse(DictionaryItem, filter, field_map={"Domain": "domain", "Key": "key", "Text": "text"})
        except FilterSyntaxError as exc:
            return _err(400, "VALIDATION_ERROR", f"Unsupported $filter expression: {exc}")
        if expr is not None:
            query = query.filter(expr)
    rows = [_to_dict_row(r.domain, r.key, r.text) for r in query.order_by(asc(DictionaryItem.domain), asc(DictionaryItem.key)).all()]

    config_rows = [_to_dict_row(r.get("Domain", ""), r.get("Key", ""), r.get("Text", "")) for r in _dictionary_config_rows(db)]
    if filter:
        try:
            pred = parse_filter_to_predicate(filter, {"Domain": "Domain", "Key": "Key", "Text": "Text"})
        except FilterSyntaxError as exc:
            return _err(400, "VALIDATION_ERROR", f"Unsupported $filter expression: {exc}")
        config_rows = [row for row in config_rows if pred(row)]
    rows.extend(config_rows)
    rows = _apply_orderby_rows(rows, orderby or "Domain asc,Key asc")
    total = len(rows)
    bounded_top = min(int(top or DEFAULT_PAGE_SIZE), MAX_PAGE_SIZE)
    paged = rows[int(skip or 0): int(skip or 0) + bounded_top]
    return odata_payload(paged, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/PersonVHSet")
def person_vh_set(
    filter: str | None = Query(None, alias="$filter"),
    search: str | None = Query(None, alias="Search"),
    date_check: str | None = Query(None, alias="DateCheck"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    query = db.query(Person)
    expr = FilterParser.parse(Person, filter, field_map=PERSON_VH_MAP)
    if expr is not None:
        query = query.filter(expr)
    if date_check:
        active_date = _parse_odata_datetime(date_check).date()
        query = query.filter(
            Person.begda <= active_date,
            (Person.endda.is_(None)) | (Person.endda >= active_date),
        )
    query = query.order_by(asc(Person.last_name), asc(Person.first_name), asc(Person.middle_name), asc(Person.perner))
    rows = query.all()
    if search:
        sQuery = str(search).strip().lower()
        rows = [
            row for row in rows
            if sQuery in _person_full_name(row).lower() or sQuery in str(row.perner or "").lower()
        ]
    total = len(rows)
    top, skip = parse_paging(top, skip, DEFAULT_PAGE_SIZE)
    return odata_payload([_to_person_vh(row) for row in rows[skip: skip + top]], total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/RuntimeSettingsSet")
def runtime_settings_set(db: Session = Depends(get_db)):
    item = SettingsService.load_global(SettingsRepo(db))
    item.setdefault("__metadata", _entity_metadata("RuntimeSettings", "RuntimeSettingsSet", item.get("Key", "GLOBAL")))
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


@router.get(f"{SERVICE_ROOT}/CurrentUserSet")
def current_user_set(request: Request, response: Response, db: Session = Depends(get_db)):
    profile = CurrentUserService.resolve_profile(db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_collection([_to_current_user(profile)])


@router.get(f"{SERVICE_ROOT}/CurrentUserSet({{entity_key}})")
def current_user_entity(entity_key: str, request: Request, response: Response, db: Session = Depends(get_db)):
    cleaned = str(entity_key or "").strip()
    if cleaned.startswith("Key="):
        cleaned = cleaned.split("=", 1)[1]
    cleaned = cleaned.strip("\'\"")
    if cleaned and cleaned.upper() != "CURRENT":
        return _err(404, "NOT_FOUND", "Current user not found")
    profile = CurrentUserService.resolve_profile(db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_entity(_to_current_user(profile))


@router.get(f"{SERVICE_ROOT}/ChecklistCreatePermissionSet")
def checklist_create_permission_set(request: Request, response: Response, db: Session = Depends(get_db)):
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_collection([_to_create_permission(resolved_uname, db=db)])


@router.get(f"{SERVICE_ROOT}/ChecklistCreatePermissionSet({{entity_key}})")
def checklist_create_permission_entity(entity_key: str, request: Request, response: Response, db: Session = Depends(get_db)):
    cleaned = str(entity_key or "").strip()
    if cleaned.startswith("DB_KEY=") or cleaned.startswith("Key="):
        cleaned = cleaned.split("=", 1)[1]
    cleaned = cleaned.strip("\'\"")
    if cleaned and cleaned.upper() != "CURRENT":
        return _err(404, "NOT_FOUND", "Create permission not found")
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    response.headers["Cache-Control"] = "no-store"
    return odata_entity(_to_create_permission(resolved_uname, db=db))


@router.get(f"{SERVICE_ROOT}/LastChangeSet({{entity_key}})")
def last_change_entity(entity_key: str, response: Response, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    agg = _agg_changed_on(root)
    root_key_hex = _hex(root.id)
    response.headers["ETag"] = format_entity_etag(agg, root.version_number)
    return odata_entity({
        "__metadata": _entity_metadata("LastChange", "LastChangeSet", root_key_hex),
        "DB_KEY": root_key_hex,
        "AggChangedOn": format_datetime(agg)
    })


@router.get(f"{SERVICE_ROOT}/LastChangeSet")
def last_change_set(
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    filter = _normalize_root_filter_aliases(filter)
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"DB_KEY": "id"},
        filter,
        orderby,
        top,
        skip,
    )
    payload = [{
        "__metadata": _entity_metadata("LastChange", "LastChangeSet", _hex(r.id)),
        "DB_KEY": _hex(r.id),
        "AggChangedOn": format_datetime(_agg_changed_on(r))
    } for r in rows]
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
    filter = _normalize_root_filter_aliases(filter)
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"DB_KEY": "id"},
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


@router.get(f"{SERVICE_ROOT}/ChecklistPermissionSet")
def checklist_permission_set(
    request: Request,
    filter: str | None = Query(None, alias="$filter"),
    orderby: str | None = Query(None, alias="$orderby"),
    top: int = Query(DEFAULT_PAGE_SIZE, alias="$top"),
    skip: int = Query(0, alias="$skip"),
    inlinecount: str | None = Query(None, alias="$inlinecount"),
    db: Session = Depends(get_db),
):
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    filter = _normalize_root_filter_aliases(filter)
    rows, total = _apply_order_filter(
        db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)),
        ChecklistRoot,
        {"DB_KEY": "id"},
        filter,
        orderby,
        top,
        skip,
    )
    payload = [_to_permission(row, resolved_uname, db=db) for row in rows]
    return odata_payload(payload, total if inlinecount == "allpages" else None)


@router.get(f"{SERVICE_ROOT}/ChecklistPermissionSet({{entity_key}})")
def checklist_permission_entity(entity_key: str, request: Request, db: Session = Depends(get_db)):
    root, err = _load_root_or_error(db, entity_key)
    if err:
        return err
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    return odata_entity(_to_permission(root, resolved_uname, db=db))


def lock_control(payload: dict, db: Session):
    action = str(payload.get("Action") or "").upper()
    root_key = str(payload.get("DB_KEY") or "")
    session = str(payload.get("SessionGuid") or "")
    uname = CurrentUserService.resolve_uname(db=db)
    if not root_key:
        return _err(400, "VALIDATION_ERROR", "DB_KEY is required")
    def _lock_entity(ok: bool, reason: str, expires_at, is_killed: bool, action_name: str, owner: str = "", owner_session: str = "", code: str = ""):
        lock_exp = format_datetime(expires_at)
        return odata_entity({
            "Success": ok,
            "Ok": ok,
            "Code": str(code or reason or "").strip(),
            "LockExpires": lock_exp,
            "ExpiresOn": lock_exp,
            "IsKilled": is_killed,
            "ReasonCode": reason,
            "Action": action_name,
            "Owner": owner,
            "OwnerSession": owner_session,
        })

    if root_key == "__CREATE":
        if action == "HEARTBEAT":
            return _lock_entity(True, "OWNED_BY_YOU", None, False, "HEARTBEAT", uname, session, "LOCK_OK")
        if action == "RELEASE":
            return _lock_entity(True, "FREE", None, False, "RELEASED", code="LOCK_OK")
        if action == "ACQUIRE":
            return _lock_entity(True, "OWNED_BY_YOU", None, False, "ACQUIRED", uname, session, "LOCK_OK")

    root_uuid = _entity_key(root_key)
    root_exists = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_uuid, ChecklistRoot.is_deleted.isnot(True)).first()
    if not root_exists:
        if action == "RELEASE":
            return _lock_entity(True, "FREE", None, False, "RELEASED", code="LOCK_OK")
        if action == "HEARTBEAT":
            return _lock_entity(False, "FREE", None, False, "FREE", code="LOCK_MISSING")
        return _err(404, "NOT_FOUND", "Checklist not found")

    if action == "ACQUIRE":
        r = LockService.acquire(
            db,
            root_uuid,
            session,
            uname,
            payload.get("StealFrom"),
            force_takeover=bool(payload.get("ForceTakeover"))
        )
        ok = bool(r.get("success"))
        return _lock_entity(
            ok,
            "OWNED_BY_YOU" if ok else "LOCKED_BY_OTHER",
            r.get("lock_expires"),
            bool(r.get("is_killed_flag")),
            str(r.get("action") or ("ACQUIRED" if ok else "FAILED")),
            str(r.get("owner") or (uname if ok else "")),
            str(r.get("owner_session") or (session if ok else "")),
            str(r.get("code") or ""),
        )
    if action == "HEARTBEAT":
        try:
            r = LockService.heartbeat(db, root_uuid, session)
            ok = bool(r.get("success"))
            return _lock_entity(
                ok,
                "OWNED_BY_YOU" if ok else "KILLED",
                r.get("lock_expires"),
                bool(r.get("is_killed")),
                str(r.get("action") or ("HEARTBEAT" if ok else "FAILED")),
                str(r.get("owner") or (uname if ok else "")),
                str(r.get("owner_session") or (session if ok else "")),
                str(r.get("code") or ""),
            )
        except ValueError as exc:
            code = str(exc)
            if code == "LOCK_MISSING":
                return _lock_entity(False, "FREE", None, False, "FREE", code="LOCK_MISSING")
            if code == "LOCK_NOT_OWNED_BY_SESSION":
                active_lock = LockService._active_lock(db, root_uuid)
                return _lock_entity(
                    False,
                    "LOCKED_BY_OTHER",
                    LockService._lock_expires_at(active_lock),
                    False,
                    "FAILED",
                    str(active_lock.user_id or "") if active_lock else "",
                    str(active_lock.session_guid or "") if active_lock else "",
                    "LOCK_NOT_OWNED_BY_SESSION",
                )
            return _err(410, "LOCK_EXPIRED", "Lock expired")
    if action == "RELEASE":
        r = LockService.release(db, root_uuid, session)
        released = bool(r.get("released"))
        return _lock_entity(
            released,
            str(r.get("reason_code") or ("FREE" if released else "LOCKED_BY_OTHER")),
            None,
            False,
            str(r.get("action") or ("RELEASED" if released else "FAILED")),
            str(r.get("owner") or ""),
            str(r.get("owner_session") or ""),
            code=str(r.get("code") or ""),
        )
    return _err(400, "VALIDATION_ERROR", "Unsupported Action")


@router.post(f"{SERVICE_ROOT}/LockAcquire")
async def lock_acquire_function_import(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _resolve_lock_root_id(merged, root_id)
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")
    return lock_control(_lock_import_payload("ACQUIRE", resolved_root_id, resolved_session_guid, merged), db)


@router.post(f"{SERVICE_ROOT}/LockHeartbeat")
async def lock_heartbeat_function_import(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _resolve_lock_root_id(merged, root_id)
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")
    return lock_control(_lock_import_payload("HEARTBEAT", resolved_root_id, resolved_session_guid, merged), db)


@router.post(f"{SERVICE_ROOT}/LockRelease")
async def lock_release_function_import(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _resolve_lock_root_id(merged, root_id)
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")
    return lock_control(_lock_import_payload("RELEASE", resolved_root_id, resolved_session_guid, merged), db)


@router.post(f"{SERVICE_ROOT}/AutoSave")
def auto_save(payload: dict, response: Response, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    root, err = _load_root_or_error(db, _save_request_root_key(payload))
    if err:
        return err
    session_guid = _save_request_session(payload)
    if not session_guid:
        return _err(400, "VALIDATION_ERROR", "SessionGuid is required")
    try:
        LockService.validate_session_lock(db, root.id, session_guid)
    except ValueError as ex:
        if str(ex) in {"LOCK_MISSING", "LOCK_NOT_OWNED_BY_SESSION"}:
            return _err(409, str(ex), "Active lock for session is required")
        return _err(409, "LOCK_EXPIRED", "Lock expired")

    _apply_save_root(root, _save_request_root(payload), db)
    _apply_save_detail_rows(db, root, body.get("checks"), _CHECK_KIND)
    _apply_save_detail_rows(db, root, body.get("barriers"), _BARRIER_KIND)
    active_lock = LockService._active_lock(db, root.id)
    lock_refreshed_at = now_utc()
    if active_lock and str(active_lock.session_guid or "").strip() == str(session_guid or "").strip():
        active_lock.last_refresh_at = lock_refreshed_at
        active_lock.expires_at = lock_refreshed_at + LOCK_TTL
    root.changed_by = CurrentUserService.resolve_uname(db=db) or root.changed_by or "ANON"
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    active_lock = LockService._active_lock(db, root.id)
    request_id = str(uuid.uuid4())
    response.headers["sap-message"] = build_sap_message("Autosave completed", "success", code="SAVED")
    return odata_entity({
        "db_key": _hex(root.id),
        "changed_on": format_datetime(root.changed_on),
        "version_number": int(root.version_number or 0),
        "code": "LOCK_OK",
        "reason_code": "SAVED",
        "lock_refreshed": True,
        "lock_expires_at": format_datetime(LockService._lock_expires_at(active_lock)) if active_lock else "",
        "server_now": format_datetime(now_utc()),
        "request_id": request_id
    })


@router.post(f"{SERVICE_ROOT}/CreateChecklist")
def create_checklist(payload: dict, response: Response, db: Session = Depends(get_db)):
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    full = body.get("FullPayload") or body
    root_block = full.get("root") if isinstance(full, dict) else {}
    basic_values = _normalize_basic_payload(full.get("basic"), db)
    requested_id = _pick_text(root_block, "Id", "RequestId")
    requested_status = _pick_text(root_block, "Status", "status") or "DRAFT"
    user_name = CurrentUserService.resolve_uname(db=db)

    root = ChecklistRoot(
        id=str(uuid.uuid4()),
        checklist_id=requested_id or _next_checklist_id(db),
        lpc=str(basic_values.get("lpc") or ""),
        status=_normalize_status_input(requested_status),
        integration_flag=False,
        created_by=user_name,
        changed_by=user_name,
        version_number=1,
    )
    _apply_basic_payload(root, basic_values)
    root.changed_on = now_utc()
    db.add(root)
    db.flush()
    _replace_detail_rows(db, root, full.get("checks"), full.get("barriers"))
    try:
        _apply_save_attachments(db, root, full.get("attachments"))
    except ValueError:
        db.rollback()
        return _err(400, "INVALID_ATTACHMENT_PAYLOAD", "Attachment payload is invalid")
    except RuntimeError as ex:
        db.rollback()
        return ex.args[0]
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()

    out = _to_root(root, db=db)
    out["DB_KEY"] = out["DB_KEY"]
    out["AggChangedOn"] = format_datetime(_agg_changed_on(root))
    out["Message"] = "Checklist created"
    out["ReasonCode"] = "CREATED"
    response.headers["sap-message"] = build_sap_message("Checklist created", "success", code="CREATED")
    return odata_entity(out)


@router.post(f"{SERVICE_ROOT}/CopyChecklist")
async def copy_checklist(request: Request, root_id: str | None = Query(None, alias="DB_KEY"), session_guid: str | None = Query(None, alias="SessionGuid"), payload: dict | None = None, db: Session = Depends(get_db)):
    body = payload or {}
    if not body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_id = _boundary_root_key(merged, root_id, merged.get("source_parent_key"), merged.get("SourceParentKey"), merged.get("SourceUuid"))
    resolved_session_guid = session_guid or merged.get("SessionGuid") or merged.get("session_guid")
    resolved_uname = CurrentUserService.resolve_uname(db=db, request=request)
    if not resolved_root_id or not resolved_session_guid:
        return _err(400, "VALIDATION_ERROR", "DB_KEY and SessionGuid are required")

    source_root, err = _load_root_or_error(db, resolved_root_id)
    if err:
        return err

    try:
        copied_root = ChecklistService.copy(db, source_root.id, str(resolved_uname))
        lock_result = LockService.acquire(db, copied_root.id, str(resolved_session_guid).strip(), str(resolved_uname), None)
    except ValueError:
        return _err(404, "NOT_FOUND", "Checklist not found")

    if not lock_result.get("success"):
        return _err(409, "LOCK_REQUIRED", "Unable to lock copied checklist")

    AnalyticsService.mark_dirty()
    copied_root = db.query(ChecklistRoot).filter(ChecklistRoot.id == copied_root.id).first()
    response_payload = {
        "RequestId": copied_root.checklist_id or "",
        "Ok": True,
        "Success": True,
        "Message": "Checklist copied",
        "AggChangedOn": format_datetime(_agg_changed_on(copied_root)),
        "ReasonCode": "COPIED",
        "ExpiresOn": format_datetime(lock_result.get("lock_expires")),
        "LockExpires": format_datetime(lock_result.get("lock_expires")),
        "IsKilled": False,
        "Status": _status_external(copied_root.status),
        "DB_KEY": _hex(copied_root.id),
    }
    return odata_entity(response_payload)


@router.post(f"{SERVICE_ROOT}/SaveChanges")
def save_changes(payload: dict, response: Response, if_match: str | None = Header(None, alias="If-Match"), db: Session = Depends(get_db)):
    body = payload.get("Payload") if isinstance(payload.get("Payload"), dict) else payload
    root, err = _load_root_or_error(db, _save_request_root_key(payload))
    if err:
        return err
    session_guid = _save_request_session(payload)
    if not session_guid:
        return _err(400, "VALIDATION_ERROR", "SessionGuid is required")
    try:
        LockService.validate_session_lock(db, root.id, session_guid)
    except ValueError as ex:
        if str(ex) in {"LOCK_MISSING", "LOCK_NOT_OWNED_BY_SESSION"}:
            return _err(409, str(ex), "Active lock for session is required")
        return _err(409, "LOCK_EXPIRED", "Lock expired")

    _apply_save_root(root, _save_request_root(payload), db)
    _apply_save_detail_rows(db, root, body.get("checks"), _CHECK_KIND)
    _apply_save_detail_rows(db, root, body.get("barriers"), _BARRIER_KIND)
    try:
        _apply_save_attachments(db, root, body.get("attachments"))
    except ValueError as ex:
        db.rollback()
        if str(ex) == "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN":
            return _err(400, "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN", "Attachment upload must use media stream endpoint")
        return _err(400, "INVALID_ATTACHMENT_PAYLOAD", "Attachment payload is invalid")
    except RuntimeError as ex:
        db.rollback()
        return ex.args[0]
    active_lock = LockService._active_lock(db, root.id)
    lock_refreshed_at = now_utc()
    if active_lock and str(active_lock.session_guid or "").strip() == str(session_guid or "").strip():
        active_lock.last_refresh_at = lock_refreshed_at
        active_lock.expires_at = lock_refreshed_at + LOCK_TTL
    root.changed_by = CurrentUserService.resolve_uname(db=db) or root.changed_by or "ANON"
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    active_lock = LockService._active_lock(db, root.id)
    request_id = str(uuid.uuid4())
    response.headers["sap-message"] = build_sap_message("Checklist updated", "success", code="SAVED")
    return odata_entity({
        "db_key": _hex(root.id),
        "changed_on": format_datetime(root.changed_on),
        "version_number": int(root.version_number or 0),
        "code": "LOCK_OK",
        "reason_code": "SAVED",
        "lock_refreshed": True,
        "lock_expires_at": format_datetime(LockService._lock_expires_at(active_lock)) if active_lock else "",
        "server_now": format_datetime(now_utc()),
        "request_id": request_id
    })


@router.post(f"{SERVICE_ROOT}/SetChecklistStatus")
async def set_status(
    request: Request,
    response: Response,
    root_key_q: str | None = Query(None, alias="DB_KEY"),
    new_status_q: str | None = Query(None, alias="NewStatus"),
    client_agg_q: str | None = Query(None, alias="ClientAggChangedOn"),
    if_match: str | None = Header(None, alias="If-Match"),
    db: Session = Depends(get_db),
):
    # SAP UI5 callFunction (POST) sends params as URL query string.
    # Fall back to JSON body for direct POST callers.
    body: dict = {}
    try:
        body = await request.json()
    except (json.JSONDecodeError, ValueError):
        body = {}
    merged = _merge_query_and_payload(request, body)
    resolved_root_key = _boundary_root_key(merged, root_key_q)
    resolved_new_status = new_status_q or merged.get("NewStatus") or merged.get("new_status") or merged.get("Status") or merged.get("status") or ""
    resolved_client_agg = client_agg_q or merged.get("ClientAggChangedOn") or merged.get("client_agg_changed_on")

    root, err = _load_root_or_error(db, resolved_root_key)
    if err:
        return err
    try:
        _if_match_check(if_match, _agg_changed_on(root), root.version_number)
        _optimistic_check(resolved_client_agg, root)
    except ValueError as ex:
        if str(ex) == "PRECONDITION_FAILED":
            return _err(412, "PRECONDITION_FAILED", "ETag mismatch")
        if str(ex) == "VALIDATION_ERROR":
            return _err(400, "VALIDATION_ERROR", "ClientAggChangedOn is required")
        return _err(409, "CONFLICT", "AggChangedOn mismatch")

    new_status = _normalize_status_input(resolved_new_status)
    if new_status not in {"DRAFT", "REGISTERED", "CLOSED"}:
        return _err(400, "VALIDATION_ERROR", "Unsupported status")
    try:
        _validate_status_change(root, new_status)
    except ValueError:
        return _err(400, "VALIDATION_ERROR", "Invalid status transition")
    root.status = new_status
    root.changed_on = now_utc()
    db.commit()
    AnalyticsService.mark_dirty()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root.id).first()
    response.headers["sap-message"] = build_sap_message("Status updated", "success", code="STATUS_SET")
    return odata_entity({"DB_KEY": _hex(root.id), "Status": root.status, "AggChangedOn": format_datetime(_agg_changed_on(root)), "Message": "Status updated", "ReasonCode": "STATUS_SET"})


@router.get(f"{SERVICE_ROOT}/GetHierarchy")
@router.post(f"{SERVICE_ROOT}/GetHierarchy")
async def get_hierarchy(request: Request, date_check: str | None = Query(None, alias="DateCheck"), method: str | None = Query(None, alias="Method"), db: Session = Depends(get_db)):
    payload = {}
    if request.method == "POST":
        try:
            payload = await request.json()
        except (json.JSONDecodeError, ValueError):
            payload = {}
    date_value = payload.get("DateCheck") or payload.get("date") or date_check
    method_value = str(payload.get("Method") or payload.get("method") or method or "location_tree").lower()
    # Only location_tree is implemented; other values are a no-op but not an error
    if method_value not in ("location_tree", ""):
        pass  # silently ignore unknown methods, return tree anyway
    try:
        dt = _parse_odata_datetime(date_value)
    except (ValueError, TypeError, AttributeError):
        dt = now_utc()
    nodes = HierarchyService.get_tree(db, dt.date())
    return odata_entity({"DateCheck": format_datetime(datetime(dt.year, dt.month, dt.day, tzinfo=timezone.utc)), "results": nodes})


@router.post(f"{SERVICE_ROOT}/ReportExport")
def report_export(payload: dict, db: Session = Depends(get_db)):
    payload = payload if isinstance(payload, dict) else {}
    selection_mode = str(payload.get("SelectionMode") or "").strip().lower()
    entity = str(payload.get("Entity") or "screen").strip().lower() or "screen"
    export_limit = _normalize_export_limit(payload.get("Limit") or payload.get("limit"))
    root_keys = (
        payload.get("RootKeys")
        or payload.get("DBKeys")
        or payload.get("keys")
        or payload.get("ids")
        or []
    )

    if not root_keys:
        single_key = _boundary_root_key(payload)
        if single_key:
            root_keys = [single_key]

    root_keys = [_normalize_hex_key(value) for value in root_keys if _normalize_hex_key(value)]
    if root_keys and len(root_keys) > export_limit:
        return _err(400, "EXPORT_LIMIT_EXCEEDED", "Selected export exceeds configured limit")

    roots = []
    if root_keys:
        roots_by_key = {}
        for key in root_keys:
            root = db.query(ChecklistRoot).filter(ChecklistRoot.id == _entity_key(str(key)), ChecklistRoot.is_deleted.isnot(True)).first()
            if root:
                roots_by_key[str(key)] = root
        roots = [roots_by_key[key] for key in root_keys if key in roots_by_key]
    elif selection_mode == "all":
        search_contract = _normalize_export_search_contract(payload.get("SearchContract"))
        roots = [
            root for root in db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)).all()
            if _search_contract_matches(root, search_contract, db)
        ]

    # Preload CHECK/BARRIER dictionary texts once instead of one query per row - the loop
    # below can run over thousands of check/barrier rows for a single "all" export.
    check_dict_texts = {item.key: item.text for item in db.query(DictionaryItem).filter(DictionaryItem.domain == "CHECK").all()}
    barrier_dict_texts = {item.key: item.text for item in db.query(DictionaryItem).filter(DictionaryItem.domain == "BARRIER").all()}

    rows = []
    for root in roots:
        base = _to_search(root, db=db)
        checks = [_to_check(c) for c in root.checks]
        barriers = [_to_barrier(b) for b in root.barriers]
        base_row = {
            "DB_KEY": base["DB_KEY"],
            "Id": base["Id"],
            "Lpc": base.get("Lpc", ""),
            "LpcText": base.get("LpcText", ""),
            "Profession": base.get("Profession", ""),
            "ProfessionText": base.get("ProfessionText", ""),
            "LocationKey": base.get("LocationKey", ""),
            "Status": base.get("Status", ""),
            "EquipName": base.get("EquipName", ""),
            "ChangedOn": base.get("ChangedOn", ""),
            "DateCheck": base["DateCheck"],
        }

        if entity == "screen":
            rows.append({
                **base_row,
                "ItemType": "ROOT",
                "Num": 0,
                "Text": "", "Comment": "", "Result": None
            })
            continue

        if entity == "check":
            for c in checks:
                key = str(c["ChecksNum"])
                rows.append({
                    **base_row,
                    "ItemType": "CHECK",
                    "Num": c["ChecksNum"],
                    "Text": check_dict_texts[key] if key in check_dict_texts else c.get("Text", ""),
                    "Comment": c["Comment"],
                    "Result": c["Result"]
                })
            continue

        if entity == "barrier":
            for b in barriers:
                key = str(b["BarriersNum"])
                rows.append({
                    **base_row,
                    "ItemType": "BARRIER",
                    "Num": b["BarriersNum"],
                    "Text": barrier_dict_texts[key] if key in barrier_dict_texts else b.get("Text", ""),
                    "Comment": b["Comment"],
                    "Result": b["Result"]
                })
            continue

        for c in checks:
            key = str(c["ChecksNum"])
            rows.append({
                **base_row,
                "ItemType": "CHECK", "Num": c["ChecksNum"],
                "Text": check_dict_texts[key] if key in check_dict_texts else c.get("Text", ""),
                "Comment": c["Comment"], "Result": c["Result"]
            })
        for b in barriers:
            key = str(b["BarriersNum"])
            rows.append({
                "DB_KEY": base["DB_KEY"], "Id": base["Id"],
                "Lpc": base.get("Lpc", ""), "LpcText": base.get("LpcText", ""),
                "Profession": base.get("Profession", ""), "ProfessionText": base.get("ProfessionText", ""),
                "DateCheck": base["DateCheck"],
                "ItemType": "BARRIER", "Num": b["BarriersNum"],
                "Text": barrier_dict_texts[key] if key in barrier_dict_texts else b.get("Text", ""),
                "Comment": b["Comment"], "Result": b["Result"]
            })
    if len(rows) > export_limit:
        return _err(400, "EXPORT_LIMIT_EXCEEDED", "Export exceeds configured limit")
    return odata_collection(rows)


@router.get(f"{SERVICE_ROOT}/AttachmentFolderSet")
def attachment_folder_set(filter: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    folder_keys = set()
    rows = []
    for entry in db.query(AttachmentEntry).order_by(asc(AttachmentEntry.folder_key), asc(AttachmentEntry.changed_on)).all():
        if not str(entry.folder_key or "").strip() or entry.folder_key in folder_keys:
            continue
        if not _attachment_matches_filter(entry, filter):
            continue
        folder_keys.add(entry.folder_key)
        folder_key_str = str(entry.folder_key)
        rows.append({
            "__metadata": _entity_metadata("AttachmentFolder", "AttachmentFolderSet", folder_key_str),
            "FolderKey": folder_key_str,
            "DB_KEY": _hex(entry.root_id),
            "Title": folder_key_str,
            "CreatedOn": format_datetime(entry.created_on),
            "ChangedOn": format_datetime(entry.changed_on),
        })
    folders = rows
    return odata_payload(folders)


@router.get(f"{SERVICE_ROOT}/AttachmentSet")
def attachment_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    rows = [
        _to_attachment(entry, db=db)
        for entry in db.query(AttachmentEntry).order_by(desc(AttachmentEntry.changed_on), desc(AttachmentEntry.created_on)).all()
        if _attachment_matches_filter(entry, filter)
    ]
    return odata_payload(rows)


@router.get(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})")
def attachment_get(entity_key: str, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    return odata_entity(_to_attachment(entry, db=db))


@router.get(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})/$value")
def attachment_value(entity_key: str, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    payload = Path(entry.storage_path or "").read_bytes() if Path(entry.storage_path or "").exists() else b""
    return Response(content=payload, media_type=str(entry.mime_type or "application/octet-stream"))


@router.post(f"{SERVICE_ROOT}/AttachmentSet")
async def attachment_create(
    request: Request,
    db_key: str | None = Header(None, alias="X-DB-Key"),
    parent_key: str | None = Header(None, alias="X-Parent-Key"),
    folder_key: str | None = Header(None, alias="X-Folder-Key"),
    category_key: str | None = Header(None, alias="X-Category-Key"),
    attachment_key: str | None = Header(None, alias="X-Attachment-Key"),
    description: str | None = Header(None, alias="X-Description"),
    file_name: str | None = Header(None, alias="X-File-Name"),
    slug: str | None = Header(None, alias="Slug"),
    content_type: str | None = Header(None),
    db: Session = Depends(get_db)
):
    body = {}
    raw_body = await request.body()

    if not raw_body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}

    if raw_body:
        body = _media_upload_payload(
            db_key=_boundary_root_key({"DB_KEY": db_key}, db_key),
            parent_key=_boundary_parent_key({"PARENT_KEY": parent_key}, parent_key) or _boundary_root_key({"DB_KEY": db_key}, db_key),
            folder_key=str(folder_key or "").strip(),
            category_key=str(category_key or "").strip() or "GEN",
            file_name=_decode_slug(slug) or str(file_name or "").strip() or "attachment",
            mime_type=str(content_type or "application/octet-stream").split(";", 1)[0].strip() or "application/octet-stream",
            description=str(description or "").strip(),
            body=raw_body,
            attachment_key=str(attachment_key or "").strip(),
        )

    root, err = _load_root_or_error(db, _boundary_root_key(body, body.get("PARENT_KEY")))
    if err:
        return err
    try:
        _apply_save_attachments(db, root, [body], allow_media_content=True)
    except ValueError as ex:
        db.rollback()
        if str(ex) == "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN":
            return _err(400, "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN", "Attachment upload must use media stream endpoint")
        return _err(400, "INVALID_ATTACHMENT_PAYLOAD", "Attachment payload is invalid")
    except RuntimeError as ex:
        db.rollback()
        return ex.args[0]
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.root_id == root.id).order_by(desc(AttachmentEntry.created_on), desc(AttachmentEntry.changed_on)).first()
    return odata_entity(_to_attachment(entry, db=db))


@router.patch(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})")
def attachment_update(entity_key: str, payload: dict, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    if "Description" in payload:
        entry.description = str(payload.get("Description") or "").strip()
    if "CategoryKey" in payload:
        entry.category_key = str(payload.get("CategoryKey") or entry.category_key or "GEN").strip() or "GEN"
    if payload.get("FileName"):
        entry.file_name = str(payload.get("FileName") or entry.file_name or "").strip()
    entry.changed_on = now_utc()
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == entry.root_id).first()
    if root:
        root.changed_on = now_utc()
        root.version_number = int(root.version_number or 0) + 1
    db.commit()
    return odata_entity(_to_attachment(entry, db=db))


@router.delete(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})")
def attachment_delete(entity_key: str, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    path = Path(entry.storage_path or "")
    if path.exists():
        path.unlink()
    root_uuid = entry.root_id
    db.delete(entry)
    root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_uuid).first() if root_uuid else None
    if root:
        root.changed_on = now_utc()
    db.commit()
    return Response(status_code=204)
