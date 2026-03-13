import os
from datetime import timedelta
from pathlib import Path


def _env_flag(name: str, default: bool) -> bool:
    raw_value = str(os.getenv(name, "") or "").strip().lower()
    if not raw_value:
        return default
    return raw_value in {"1", "true", "yes", "on"}

DATABASE_URL = "sqlite:///" + str((Path(__file__).resolve().parent / "gateway.db").as_posix())
APP_PROFILE = str(os.getenv("PCCT_PROFILE", "local") or "local").strip().lower()
IS_LOCAL_PROFILE = APP_PROFILE == "local"
FRONTEND_TIMER_TEST_PROFILE = {
    "heartbeat_ms": 3000,
    "lock_status_ms": 2000,
    "gcd_ms": 5000,
    "idle_ms": 600000,
    "autosave_interval_ms": 4000,
    "autosave_debounce_ms": 400,
    "network_grace_ms": 4000,
    "cache_fresh_ms": 2000,
    "cache_stale_ok_ms": 4000,
    "analytics_refresh_ms": 5000,
}
LOCK_TTL = timedelta(seconds=15)
LOCK_HEARTBEAT_INTERVAL_SECONDS = 3
LOCK_CLEANUP_INTERVAL_SECONDS = 3
METADATA_REFRESH_INTERVAL_SECONDS = 30
LOCK_KILLED_RETENTION = timedelta(seconds=15)
DEFAULT_PAGE_SIZE = 50
MAX_SUGGEST_RESULTS = 20


CORS_ALLOWED_ORIGINS = [
    "http://localhost:8080",
    "http://127.0.0.1:8080",
]

EXPOSE_NON_CANONICAL_ROUTES = False
ALLOW_MOCK_USER_HEADER = IS_LOCAL_PROFILE and _env_flag("PCCT_ALLOW_MOCK_USER_HEADER", True)
AUTO_MUTATE_SCHEMA_ON_STARTUP = IS_LOCAL_PROFILE and _env_flag("PCCT_AUTO_MUTATE_SCHEMA", True)
AUTO_SEED_STARTUP_DATA = IS_LOCAL_PROFILE and _env_flag("PCCT_AUTO_SEED_STARTUP_DATA", True)
LOG_REQUEST_BODIES = _env_flag("PCCT_LOG_REQUEST_BODIES", False)
