import asyncio
import logging
from config import LOCK_CLEANUP_INTERVAL_SECONDS, METADATA_REFRESH_INTERVAL_SECONDS
from database import SessionLocal
from models import FrontendRuntimeSettings
from services.analytics_service import AnalyticsService
from services.lock_service import LockService
from services.metadata_cache import refresh_metadata

logger = logging.getLogger("gateway")


async def lock_cleanup_job() -> None:
    """Background job: clean up expired locks."""
    while True:
        db = SessionLocal()
        try:
            cleaned = LockService.cleanup(db)
            if cleaned:
                logger.info("Cleaned %s expired locks", cleaned)
        finally:
            db.close()
        await asyncio.sleep(LOCK_CLEANUP_INTERVAL_SECONDS)


async def metadata_refresh_job() -> None:
    """Background job: refresh OData metadata cache."""
    while True:
        try:
            refresh_metadata()
            logger.info("Service metadata cache refreshed")
        except Exception:
            logger.exception("Failed to refresh service metadata cache")
        await asyncio.sleep(METADATA_REFRESH_INTERVAL_SECONDS)


async def analytics_refresh_job(ensure_tables, ensure_settings) -> None:
    """Background job: refresh analytics cache on schedule."""
    while True:
        db = SessionLocal()
        try:
            ensure_tables()
            ensure_settings(db)
            settings_row = db.query(FrontendRuntimeSettings).first()
            interval_ms = int(getattr(settings_row, "analytics_refresh_ms", 300000) or 300000)
            if AnalyticsService.should_refresh(db, max(5, int(interval_ms / 1000))):
                AnalyticsService.refresh_cache(db, trigger="scheduler")
            sleep_seconds = 5 if AnalyticsService._dirty else max(5, min(60, int(interval_ms / 1000)))
        except Exception:
            logger.exception("Failed to refresh analytics cache")
            sleep_seconds = 10
        finally:
            db.close()
        await asyncio.sleep(sleep_seconds)
