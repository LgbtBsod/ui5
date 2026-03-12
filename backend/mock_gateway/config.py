from datetime import timedelta
from pathlib import Path

DATABASE_URL = "sqlite:///" + str((Path(__file__).resolve().parent / "gateway.db").as_posix())
FRONTEND_TIMER_TEST_PROFILE = {
    "heartbeat_ms": 3000,
    "lock_status_ms": 2000,
    "gcd_ms": 5000,
    "idle_ms": 15000,
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
ALLOW_MOCK_USER_HEADER = True
