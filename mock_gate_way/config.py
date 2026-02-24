from datetime import timedelta

DATABASE_URL = "sqlite:///./gateway.db"
LOCK_TTL = timedelta(minutes=5)
LOCK_CLEANUP_INTERVAL_SECONDS = 30
DEFAULT_PAGE_SIZE = 50
MAX_SUGGEST_RESULTS = 20
