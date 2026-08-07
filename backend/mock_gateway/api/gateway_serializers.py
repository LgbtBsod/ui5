"""OData entity serializers (_to_*) and the field maps / search-contract helpers they use.

Everything here is pure data-shaping: turn a SQLAlchemy row (or dict) into the OData
wire shape, or normalize a raw filter/export payload into something the callers below
can compare against. No permission checks, no mutation, no request/session handling -
those live in api.gateway_core (auth/lock/etag kernel) and api.gateway_mutations.
"""

import json
import re
from datetime import datetime, timezone
from urllib.parse import unquote

from sqlalchemy.orm import Session

from api.gateway_core import _entity_key
from api.gateway_helpers import HEX_ZERO_GUID, ODataSerializer
from api.gateway_operations import _dict_text, _hex, _normalize_hex_key
from api.gateway_validators import _date_ymd_from_any, _status_external
from models import (
    AttachmentEntry,
    ChecklistBarrier,
    ChecklistCheck,
    ChecklistRoot,
    ChecklistRootDraft,
    DictionaryItem,
    Person,
)
from repo.settings_repo import SettingsRepo
from services.authorization_service import AuthorizationService
from services.settings_service import DEFAULT_FRONTEND_VARIABLES, SettingsService
from utils.common_helpers import parse_date_ms
from utils.odata import SERVICE_ROOT, format_datetime
from utils.odata_datetime import date_only_to_odata
from utils.time import now_utc

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


def _datecheck_datetime(root: ChecklistRoot):
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
        matches.append(filter_id in str(root.checklist_id or "").lower() or filter_id in str(root_id_hex or "").lower())
    if filter_date_from:
        matches.append(
            bool(root_date) and root_date >= filter_date_from and (not filter_date_to or root_date <= filter_date_to)
        )
    if filter_location_key:
        matches.append(filter_location_key in root_location_key.lower())
    if filter_lpc:
        matches.append(filter_lpc in root_lpc.lower() or filter_lpc in root_lpc_text.lower())
    if filter_profession:
        matches.append(
            filter_profession in root_profession.lower() or filter_profession in root_profession_text.lower()
        )
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
        return _hex(row.root_id) == _normalize_hex_key(root_match.group(1)) or str(row.root_id or "") == _entity_key(
            root_match.group(1)
        )
    key_match = re.search(r"(?:AttachmentKey|Key)\s+eq\s+'([^']+)'", expr, flags=re.IGNORECASE)
    if key_match:
        return str(row.id or "") == _entity_key(key_match.group(1))
    return True


def _to_attachment(entry: AttachmentEntry, db: Session | None = None) -> dict:
    entry_key = _hex(entry.id)
    category_key = str(entry.category_key or "GEN")
    category_text = category_key
    if db is not None:
        item = (
            db.query(DictionaryItem)
            .filter(DictionaryItem.domain == "ATF_CAT", DictionaryItem.key == category_key)
            .first()
        )
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
        },
    )


def _rate(items, pred) -> float:
    if not items:
        return 100.0
    ok = sum(1 for item in items if pred(item))
    return round((ok / len(items)) * 100, 2)


def _person_full_name(person: Person) -> str:
    return " ".join(
        [
            str(person.last_name or "").strip(),
            str(person.first_name or "").strip(),
            str(person.middle_name or "").strip(),
        ]
    ).strip()


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
        },
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
        },
    )


def _to_root(root: ChecklistRoot, db: Session | None = None, draft_map: dict | None = None) -> dict:
    """draft_map (optional): a prefetched {active_id: has_draft_bool} dict, so list
    endpoints (ChecklistRootSet GET collection) can batch the HasDraftEntity lookup
    instead of running one extra query per row. Single-entity GET can omit it (falls
    back to a per-row query)."""
    s = _to_search(root, db=db)
    if draft_map is not None:
        has_draft = bool(draft_map.get(root.id))
    elif db is not None:
        has_draft = bool(db.query(ChecklistRootDraft.id).filter(ChecklistRootDraft.active_id == root.id).first())
    else:
        has_draft = False
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
            "ActiveUUID": s["DB_KEY"],
            "DraftUUID": HEX_ZERO_GUID,
            "IsActiveEntity": True,
            "HasActiveEntity": True,
            "HasDraftEntity": has_draft,
        },
    )


def _to_root_draft(draft: ChecklistRootDraft, db: Session) -> dict:
    """Serializes a ChecklistRootDraft row (create-draft or edit-draft-in-progress).
    Children (ChecklistCheck/ChecklistBarrier) stay non-draft-aware - looked up directly
    by root_id, same rows the active side would see, no rollback on discard (matches
    redux/serve.py's reference scope)."""
    active_hex = _hex(draft.active_id) if draft.active_id else HEX_ZERO_GUID
    draft_hex = _hex(draft.id)
    checks = db.query(ChecklistCheck).filter(ChecklistCheck.root_id == draft.root_id).all()
    barriers = db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == draft.root_id).all()
    entity = ODataSerializer.build_entity(
        "ChecklistRoot",
        "ChecklistRootSet",
        draft_hex,
        {
            "DB_KEY": active_hex if draft.active_id else draft_hex,
            "Key": active_hex if draft.active_id else draft_hex,
            "RequestId": draft.checklist_id or "",
            "Id": draft.checklist_id or "",
            "ChangedOn": format_datetime(draft.changed_on),
            "CreatedOn": format_datetime(draft.created_on),
            "VersionNumber": 0,
            "Status": _status_external(draft.status),
            "HasFailedChecks": any((c.status or "").upper() == "FAIL" for c in checks),
            "HasFailedBarriers": any(not bool(b.is_active) for b in barriers),
            "SuccessChecksRate": _rate(checks, lambda c: (c.status or "").upper() != "FAIL"),
            "SuccessBarriersRate": _rate(barriers, lambda b: bool(b.is_active)),
            "ChecksTotal": len(checks),
            "BarriersTotal": len(barriers),
            "ActiveUUID": active_hex,
            "DraftUUID": draft_hex,
            "IsActiveEntity": False,
            "HasActiveEntity": bool(draft.active_id),
            "HasDraftEntity": False,
        },
    )
    entity["__metadata"] = {
        "type": entity["__metadata"]["type"],
        "uri": f"{SERVICE_ROOT}/ChecklistRootSet(ActiveUUID='{active_hex}',DraftUUID='{draft_hex}')",
    }
    return entity


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
        },
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
        },
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
        },
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
        group = groups.setdefault(
            group_key,
            {
                "scopeKind": scope_kind,
                "scopeValue": scope_value.upper() if scope_kind == "bukrs" else "ALL",
                "permissionCodes": [],
            },
        )
        if code not in group["permissionCodes"]:
            group["permissionCodes"].append(code)
    result = list(groups.values())
    for item in result:
        item["permissionCodes"] = sorted(item.get("permissionCodes") or [], key=lambda value: order.get(value, 99))
    result.sort(
        key=lambda item: (
            0 if str(item.get("scopeKind") or "all") == "all" else 1,
            str(item.get("scopeValue") or "ALL"),
        )
    )
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
            "CanCreate": "01" in permission_codes,
            "CanView": "03" in permission_codes,
            "CanEdit": "02" in permission_codes,
            "CanDelete": "06" in permission_codes,
            "SummaryText": _current_user_summary(profile),
        },
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
        },
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
        },
    )
