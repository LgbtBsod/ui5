"""
Payload validation and normalization: input sanitization, schema verification.
"""
from sqlalchemy.orm import Session
from models import ChecklistRoot


def _normalize_status_input(status: str | None) -> str:
    """
    Normalize any status input to internal 2-digit code.

    Accepts both external names (DRAFT, REGISTERED, CLOSED) and internal codes (01, 02, 03).
    Always returns internal format for database consistency.

    Args:
        status: Any status format (case-insensitive, whitespace-trimmed)

    Returns:
        Internal code: '01' (DRAFT), '02' (REGISTERED), '03' (CLOSED)
        Defaults to '01' (DRAFT) if input unrecognized

    Examples:
        >>> _normalize_status_input('DRAFT')
        '01'
        >>> _normalize_status_input('02')
        '02'
        >>> _normalize_status_input('unknown')
        '01'  # Fallback to DRAFT
    """
    mapping = {
        "DRAFT": "01",
        "01": "01",
        "REGISTERED": "02",
        "02": "02",
        "CLOSED": "03",
        "03": "03",
    }
    return mapping.get(str(status or "").strip(), "01")


def _status_external(status: str | None) -> str:
    """Map internal 2-digit status code to its external name. Inverse of _normalize_status_input."""
    mapping = {"01": "DRAFT", "02": "REGISTERED", "03": "CLOSED"}
    return mapping.get(str(status or "").strip(), "DRAFT")


def _pick_text(payload: dict | None, *keys) -> str:
    """Extract first non-empty string from payload keys (fallback chain)."""
    if not payload or not isinstance(payload, dict):
        return ""
    for key in keys:
        value = payload.get(key, "")
        if value and isinstance(value, str):
            return str(value).strip()
    return ""


def _pick_bool(payload: dict | None, *keys, default: bool = True) -> bool:
    """Extract first boolean from payload keys."""
    if not payload or not isinstance(payload, dict):
        return default
    for key in keys:
        value = payload.get(key)
        if value is not None:
            return bool(value)
    return default


def _pick_first_present(payload: dict | None, *keys):
    """Extract first non-null value from payload keys."""
    if not payload or not isinstance(payload, dict):
        return None
    for key in keys:
        if key in payload and payload[key] is not None:
            return payload[key]
    return None


def _coerce_int(value, default: int) -> int:
    """Coerce value to int with fallback."""
    try:
        return int(value or default)
    except (TypeError, ValueError):
        return default


def _normalize_basic_payload(basic: dict | None, db: Session) -> dict:
    """Normalize and validate basic entity payload."""
    if not basic or not isinstance(basic, dict):
        return {}

    return {
        "checklist_id": _pick_text(basic, "ChecklistId", "Id"),
        "lpc": _pick_text(basic, "Lpc"),
        "lpc_text": _pick_text(basic, "LpcText"),
        "status": _normalize_status_input(_pick_text(basic, "Status")),
        "date": _pick_text(basic, "Date"),
        "time_check": _pick_text(basic, "TimeCheck"),
        "equipment": _pick_text(basic, "Equipment"),
        "bukrs": _pick_text(basic, "Bukrs"),
        "observer_fullname": _pick_text(basic, "ObserverFullname"),
        "observer_perner": _pick_text(basic, "ObserverPerner"),
        "observer_position": _pick_text(basic, "ObserverPosition"),
        "observer_orgunit": _pick_text(basic, "ObserverOrgunit"),
    }


def _apply_basic_payload(root: ChecklistRoot, basic: dict) -> None:
    """Apply normalized basic payload to root entity."""
    if basic.get("checklist_id"):
        root.checklist_id = basic["checklist_id"]
    if basic.get("lpc"):
        root.lpc = basic["lpc"]
    if basic.get("lpc_text"):
        root.lpc_text = basic["lpc_text"]
    if basic.get("status"):
        root.status = basic["status"]
    if basic.get("date"):
        root.date = basic["date"]
    if basic.get("time_check"):
        root.time_check = basic["time_check"]
    if basic.get("equipment"):
        root.equipment = basic["equipment"]
    if basic.get("bukrs"):
        root.bukrs = basic["bukrs"]
    if basic.get("observer_fullname"):
        root.observer_fullname = basic["observer_fullname"]


def _normalize_child_rows(items, number_field: str) -> list[dict]:
    """Normalize and validate child entity rows (checks/barriers)."""
    if not items or not isinstance(items, (list, tuple)):
        return []

    normalized = []
    for idx, item in enumerate(items):
        if not isinstance(item, dict):
            continue
        normalized.append({
            **item,
            number_field: _coerce_int(item.get(number_field), idx + 1),
        })
    return normalized


def _date_ymd_from_any(value) -> str:
    """Extract YYYY-MM-DD from various date formats."""
    if not value:
        return ""
    raw = str(value)
    if raw.startswith("/Date("):
        try:
            ms = int(raw[6:-2].split("+")[0].split("-")[0])
            from datetime import datetime, timezone
            dt = datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
            return dt.strftime("%Y-%m-%d")
        except Exception:
            return ""
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    return raw.split("T", 1)[0][:10] if raw else ""


def _next_checklist_id(db: Session) -> str:
    """Generate next checklist ID based on existing count."""
    from models import ChecklistRoot
    count = db.query(ChecklistRoot).count()
    return f"CHK-{count + 1:05d}"
