from __future__ import annotations

from datetime import datetime, timezone
import uuid

LOCK_TTL_SECONDS = 5 * 60
HEARTBEAT_INTERVAL_SECONDS = 4 * 60


def now() -> datetime:
    return datetime.now(timezone.utc)


def generate_uuid() -> str:
    return str(uuid.uuid4())


def iso(dt: datetime) -> str:
    return dt.astimezone(timezone.utc).isoformat()


db = {
    "roots": {},
    "checks": {},
    "barriers": {},
    "locks": {},
    "error_flags": {
        "force_500": False,
    },
}
