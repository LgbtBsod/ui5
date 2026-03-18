import json
import logging

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from models import FrontendRuntimeSettings
from services.settings_service import DEFAULT_REQUIRED_FIELDS, DEFAULT_UPLOAD_POLICY

router = APIRouter(tags=["Settings"])
logger = logging.getLogger("gateway.settings")


def _payload(o: FrontendRuntimeSettings):
    raw_required = getattr(o, "required_fields_json", "") or ""
    try:
        required_fields = json.loads(raw_required) if raw_required else list(DEFAULT_REQUIRED_FIELDS)
    except Exception:
        required_fields = list(DEFAULT_REQUIRED_FIELDS)
    return {
        "environment": o.environment,
        "timers": {
            "heartbeatMs": int(o.heartbeat_ms or 270000),
            "lockStatusMs": int(o.lock_status_ms or 60000),
            "gcdMs": int(o.gcd_ms or 300000),
            "idleMs": int(o.idle_ms or 570000),
            "autoSaveIntervalMs": int(o.autosave_interval_ms or 150000),
            "autoSaveDebounceMs": int(o.autosave_debounce_ms or 1200),
            "lockRefreshCooldownMs": int(o.lock_refresh_cooldown_ms or 150000),
            "networkGraceMs": int(o.network_grace_ms or 60000),
            "cacheToleranceMs": int(o.cache_tolerance_ms or 5500),
            "analyticsRefreshMs": int(o.analytics_refresh_ms or 900000),
        },
        "search": {"defaultMaxResults": 100, "growingThreshold": 10},
        "requiredFields": required_fields,
        "source": "settings_table",
    }


@router.get("/FrontendRuntimeSettings")
def frontend_runtime_settings(db: Session = Depends(get_db)):
    row = db.query(FrontendRuntimeSettings).order_by(FrontendRuntimeSettings.changed_on.desc()).first()
    if not row:
        row = FrontendRuntimeSettings(
            environment="default",
            required_fields_json=json.dumps(DEFAULT_REQUIRED_FIELDS),
            upload_policy_json=json.dumps(DEFAULT_UPLOAD_POLICY)
        )
        db.add(row)
        db.commit()
        db.refresh(row)
    data = _payload(row)
    logger.info("FrontendRuntimeSettings payload=%s", data)
    return data
