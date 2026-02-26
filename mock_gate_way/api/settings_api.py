import logging

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from models import FrontendRuntimeSettings

router = APIRouter(tags=["Settings"])
logger = logging.getLogger("gateway.settings")


def _payload(o: FrontendRuntimeSettings):
    return {
        "environment": o.environment,
        "timers": {
            "heartbeatMs": int(o.heartbeat_ms or 240000),
            "lockStatusMs": int(o.lock_status_ms or 60000),
            "gcdMs": int(o.gcd_ms or 300000),
            "idleMs": int(o.idle_ms or 600000),
            "autoSaveIntervalMs": int(o.autosave_interval_ms or 60000),
            "autoSaveDebounceMs": int(o.autosave_debounce_ms or 30000),
            "networkGraceMs": int(o.network_grace_ms or 60000),
            "cacheFreshMs": int(o.cache_fresh_ms or 30000),
            "cacheStaleOkMs": int(o.cache_stale_ok_ms or 90000),
            "analyticsRefreshMs": int(o.analytics_refresh_ms or 900000),
        },
        "search": {"defaultMaxResults": 100, "growingThreshold": 10},
        "requiredFields": [
            "/basic/date",
            "/basic/time",
            "/basic/timezone",
            "/basic/OBSERVER_FULLNAME",
            "/basic/OBSERVED_FULLNAME",
            "/basic/LOCATION_KEY",
            "/basic/LPC_KEY",
            "/basic/PROF_KEY",
        ],
        "source": "settings_table",
    }


@router.get("/FrontendRuntimeSettings")
def frontend_runtime_settings(db: Session = Depends(get_db)):
    row = db.query(FrontendRuntimeSettings).order_by(FrontendRuntimeSettings.changed_on.desc()).first()
    if not row:
        row = FrontendRuntimeSettings(environment="default")
        db.add(row)
        db.commit()
        db.refresh(row)
    data = _payload(row)
    logger.info("FrontendRuntimeSettings payload=%s", data)
    return data
