from datetime import datetime, timedelta

from sqlalchemy.orm import Session

from models import LockEntry, LockLog

LOCK_TTL_SECONDS = 300


def now() -> datetime:
    return datetime.utcnow()


class LockService:
    @staticmethod
    def acquire(db: Session, pcct_uuid: str, user_id: str) -> LockEntry:
        existing = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.is_killed.is_(False),
        ).first()

        if existing and existing.expires_at and existing.expires_at > now():
            if existing.user_id == user_id:
                return existing
            raise Exception("LOCKED_BY_OTHER")

        if existing:
            existing.is_killed = True

        lock = LockEntry(
            pcct_uuid=pcct_uuid,
            user_id=user_id,
            locked_at=now(),
            last_heartbeat=now(),
            expires_at=now() + timedelta(seconds=LOCK_TTL_SECONDS),
        )

        db.add(lock)
        db.add(LockLog(pcct_uuid=pcct_uuid, user_id=user_id, action="ACQUIRE"))
        db.commit()
        db.refresh(lock)
        return lock

    @staticmethod
    def heartbeat(db: Session, pcct_uuid: str, user_id: str) -> dict:
        lock = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.user_id == user_id,
        ).first()

        if not lock:
            raise Exception("NO_LOCK")
        if lock.is_killed:
            return {"status": "KILLED"}
        if lock.expires_at and lock.expires_at < now():
            lock.is_killed = True
            db.commit()
            return {"status": "EXPIRED"}

        lock.last_heartbeat = now()
        lock.expires_at = now() + timedelta(seconds=LOCK_TTL_SECONDS)
        db.commit()
        return {"status": "OK"}

    @staticmethod
    def release(db: Session, pcct_uuid: str, user_id: str) -> None:
        lock = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.user_id == user_id,
            LockEntry.is_killed.is_(False),
        ).first()

        if not lock:
            raise Exception("NO_ACTIVE_LOCK")

        lock.is_killed = True
        db.add(LockLog(pcct_uuid=pcct_uuid, user_id=user_id, action="RELEASE"))
        db.commit()

    @staticmethod
    def steal(db: Session, pcct_uuid: str, user_id: str) -> LockEntry:
        existing = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.is_killed.is_(False),
        ).first()

        if not existing:
            return LockService.acquire(db, pcct_uuid, user_id)
        if existing.user_id == user_id:
            return existing

        existing.is_killed = True
        db.add(LockLog(pcct_uuid=pcct_uuid, user_id=user_id, action="STEAL"))

        new_lock = LockEntry(
            pcct_uuid=pcct_uuid,
            user_id=user_id,
            locked_at=now(),
            last_heartbeat=now(),
            expires_at=now() + timedelta(seconds=LOCK_TTL_SECONDS),
        )
        db.add(new_lock)
        db.commit()
        db.refresh(new_lock)
        return new_lock

    @staticmethod
    def cleanup(db: Session) -> int:
        expired = db.query(LockEntry).filter(
            LockEntry.is_killed.is_(False),
            LockEntry.expires_at < now(),
        ).all()

        for lock in expired:
            lock.is_killed = True
            db.add(LockLog(pcct_uuid=lock.pcct_uuid, user_id=lock.user_id, action="CLEANUP"))

        db.commit()
        return len(expired)

    @staticmethod
    def validate_lock(db: Session, pcct_uuid: str, user_id: str) -> bool:
        lock = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.user_id == user_id,
            LockEntry.is_killed.is_(False),
        ).first()

        if not lock:
            raise Exception("NO_VALID_LOCK")

        if lock.expires_at and lock.expires_at < now():
            lock.is_killed = True
            db.commit()
            raise Exception("LOCK_EXPIRED")

        return True
