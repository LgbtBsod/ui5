from __future__ import annotations

from sqlalchemy.orm import Session

from services.lock_service import LockService


class LockRepo:
    def __init__(self, db: Session):
        self.db = db

    def acquire(self, root_id: str, session_guid: str, user: str, steal_from: str | None = None):
        return LockService.acquire(self.db, root_id, session_guid, user, steal_from)

    def heartbeat(self, root_id: str, session_guid: str):
        return LockService.heartbeat(self.db, root_id, session_guid)

    def release(self, root_id: str, session_guid: str):
        return LockService.release(self.db, root_id, session_guid)

    def status(self, root_id: str, session_guid: str):
        return LockService.status(self.db, root_id, session_guid)
