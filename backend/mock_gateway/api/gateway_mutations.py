"""SaveChanges/AutoSave row-mutation helpers: parsing the save-request envelope and
upserting ChecklistRoot/Check/Barrier rows from it."""
import uuid

from sqlalchemy.orm import Session

from models import ChecklistRoot
from utils.time import now_utc
from api.gateway_operations import _dict_text
from api.gateway_validators import (
    _pick_text, _pick_bool, _coerce_int, _date_ymd_from_any,
    _pick_first_present, _normalize_status_input,
)
from api.gateway_core import _entity_key, _boundary_root_key
from api.gateway_detail_kind import _DetailKind


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
