import os
from datetime import timedelta
from pathlib import Path


def _env_flag(name: str, default: bool) -> bool:
    raw_value = str(os.getenv(name, "") or "").strip().lower()
    if not raw_value:
        return default
    return raw_value in {"1", "true", "yes", "on"}


DATABASE_URL = str(
    os.getenv("PCCT_DATABASE_URL") or ("sqlite:///" + str((Path(__file__).resolve().parent / "gateway.db").as_posix()))
)
APP_PROFILE = str(os.getenv("PCCT_PROFILE", "production") or "production").strip().lower()
IS_LOCAL_PROFILE = APP_PROFILE == "local"
FRONTEND_TIMER_PROFILES = {
    "production": {
        "heartbeat_ms": 270000,
        "lock_status_ms": 60000,
        "gcd_ms": 300000,
        "idle_ms": 570000,
        "autosave_interval_ms": 150000,
        "autosave_debounce_ms": 1200,
        "lock_refresh_cooldown_ms": 150000,
        "network_grace_ms": 60000,
        "cache_tolerance_ms": 5500,
        "analytics_refresh_ms": 900000,
    },
    "test": {
        "heartbeat_ms": 3000,
        "lock_status_ms": 2000,
        "gcd_ms": 5000,
        "idle_ms": 20000,
        "autosave_interval_ms": 4000,
        "autosave_debounce_ms": 400,
        "lock_refresh_cooldown_ms": 4000,
        "network_grace_ms": 4000,
        "cache_tolerance_ms": 5500,
        "analytics_refresh_ms": 5000,
    },
}
FRONTEND_TIMER_PROFILE = FRONTEND_TIMER_PROFILES["test" if APP_PROFILE == "test" else "production"]
LOCK_TTL = timedelta(seconds=600)
LOCK_HEARTBEAT_INTERVAL_SECONDS = int(FRONTEND_TIMER_PROFILE["heartbeat_ms"] / 1000)
LOCK_CLEANUP_INTERVAL_SECONDS = 900
METADATA_REFRESH_INTERVAL_SECONDS = 30
LOCK_KILLED_RETENTION = timedelta(seconds=3600)
DEFAULT_PAGE_SIZE = 50
MAX_PAGE_SIZE = int(os.getenv("PCCT_MAX_PAGE_SIZE", "1000"))
MAX_SUGGEST_RESULTS = 20
# Each $batch operation spins up its own in-process ASGI sub-request (and, inside a
# changeset, a SAVEPOINT); with no cap a single request body could fan out into an
# unbounded number of these - a resource-exhaustion vector unique to $batch.
MAX_BATCH_OPERATIONS = int(os.getenv("PCCT_MAX_BATCH_OPERATIONS", "100"))

RATE_LIMIT_REQUESTS_PER_WINDOW = int(os.getenv("PCCT_RATE_LIMIT_REQUESTS", "100"))
RATE_LIMIT_WINDOW_SECONDS = int(os.getenv("PCCT_RATE_LIMIT_WINDOW_SECONDS", "60"))
RATE_LIMIT_TRUSTED_PROXIES = {ip.strip() for ip in os.getenv("PCCT_TRUSTED_PROXIES", "").split(",") if ip.strip()}


CORS_ALLOWED_ORIGINS = [
    "http://localhost:8080",
    "http://127.0.0.1:8080",
]

EXPOSE_NON_CANONICAL_ROUTES = False
ALLOW_MOCK_USER_HEADER = IS_LOCAL_PROFILE and _env_flag("PCCT_ALLOW_MOCK_USER_HEADER", False)
AUTO_MUTATE_SCHEMA_ON_STARTUP = IS_LOCAL_PROFILE and _env_flag("PCCT_AUTO_MUTATE_SCHEMA", True)
AUTO_SEED_STARTUP_DATA = IS_LOCAL_PROFILE and _env_flag("PCCT_AUTO_SEED_STARTUP_DATA", True)
LOG_REQUEST_BODIES = _env_flag("PCCT_LOG_REQUEST_BODIES", False)

# Show a one-time local GUI login prompt at real server startup to set the mock "current
# user" (see bootstrap.prompt_and_store_login / services.current_user_service), instead of
# silently defaulting to a fixed "operator" identity.
#
# This flag only covers the explicit opt-out (PCCT_PROMPT_LOGIN_ON_STARTUP=0); the pytest
# detection deliberately does NOT live here. config.py is imported once at process start,
# often before pytest has set PYTEST_CURRENT_TEST (that's set per-test, not at collection
# time), so baking "not running under pytest" into a module-level constant here would freeze
# a stale True and still pop a blocking GUI dialog during automated test runs, hanging the
# whole suite. bootstrap.prompt_and_store_login() re-checks "pytest" in sys.modules at call
# time instead, which is reliable regardless of import ordering.
PROMPT_LOGIN_ON_STARTUP_ENABLED = _env_flag("PCCT_PROMPT_LOGIN_ON_STARTUP", True)
