"""
Common helper functions shared across multiple modules.
Single source of truth for: date parsing, policy loading.
"""

import json
from datetime import datetime, timedelta, timezone

from sqlalchemy.orm import Session

from repo.settings_repo import SettingsRepo
from services.settings_service import SettingsService


def load_upload_policy(db: Session) -> dict:
    """Load upload policy from settings."""
    payload = SettingsService.load_global(SettingsRepo(db))
    raw = payload.get("UploadPolicyJson") or "{}"
    try:
        parsed = json.loads(raw)
    except (json.JSONDecodeError, ValueError, TypeError):
        parsed = {}
    return parsed if isinstance(parsed, dict) else {}


def parse_date_ymd(value) -> str:
    """Parse various date formats to YYYY-MM-DD string."""
    if value is None:
        return ""
    raw = str(value)
    if raw.startswith("/Date(") and raw.endswith(")/"):
        # Search for the timezone-offset separator starting at index 1, not 0: a naive
        # `.split("-")[0]` would treat a legitimately negative (pre-1970) ms value's own
        # leading "-" as that separator and parse an empty string, silently discarding any
        # date before 1970 instead of returning it.
        body = raw[6:-2]
        sign_pos = max(body.find("+", 1), body.find("-", 1))
        ms_part = body if sign_pos < 0 else body[:sign_pos]
        try:
            ms = int(ms_part)
            # timedelta-from-epoch (not datetime.fromtimestamp) so pre-1970 values don't
            # crash with OSError on Windows, where the C runtime rejects negative time_t.
            dt = datetime(1970, 1, 1, tzinfo=timezone.utc) + timedelta(milliseconds=ms)
            return dt.strftime("%Y-%m-%d")
        except ValueError:
            return ""
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    if "T" in raw:
        raw = raw.split("T", 1)[0]
    return raw[:10]


def parse_date_ms(value) -> int:
    """Parse various date formats to milliseconds since epoch."""
    if value is None:
        return 0
    raw = str(value)
    if raw.startswith("/Date(") and raw.endswith(")/"):
        body = raw[6:-2]
        sign_pos = max(body.find("+", 1), body.find("-", 1))
        ms_part = body if sign_pos < 0 else body[:sign_pos]
        try:
            return int(ms_part)
        except ValueError:
            return 0
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    try:
        dt = datetime.fromisoformat(raw.replace("Z", "+00:00"))
        dt = dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)
        return int(dt.astimezone(timezone.utc).timestamp() * 1000)
    except (ValueError, TypeError, AttributeError):
        return 0
