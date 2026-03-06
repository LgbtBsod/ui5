from datetime import timedelta
from pathlib import Path

DATABASE_URL = "sqlite:///" + str((Path(__file__).resolve().parent / "gateway.db").as_posix())
LOCK_TTL = timedelta(minutes=5)
LOCK_HEARTBEAT_INTERVAL_SECONDS = 4 * 60
LOCK_CLEANUP_INTERVAL_SECONDS = 5 * 60
METADATA_REFRESH_INTERVAL_SECONDS = 5 * 60
LOCK_KILLED_RETENTION = timedelta(minutes=5)
DEFAULT_PAGE_SIZE = 50
MAX_SUGGEST_RESULTS = 20


CORS_ALLOWED_ORIGINS = [
    "http://localhost:8080",
    "http://127.0.0.1:8080",
]
