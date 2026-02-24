from datetime import timedelta

DATABASE_URL = "sqlite:///./gateway.db"
LOCK_TTL = timedelta(minutes=5)
LOCK_HEARTBEAT_INTERVAL_SECONDS = 4 * 60
LOCK_CLEANUP_INTERVAL_SECONDS = 5 * 60
LOCK_KILLED_RETENTION = timedelta(minutes=5)
DEFAULT_PAGE_SIZE = 50
MAX_SUGGEST_RESULTS = 20
