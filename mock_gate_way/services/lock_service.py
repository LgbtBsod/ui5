# services/lock_service.py

from datetime import datetime, timedelta
from sqlalchemy.orm import Session
from models import LockEntry, LockLog
import uuid


LOCK_TTL_SECONDS = 300  # 5 минут


def now():
    return datetime.utcnow()


class LockService:

    @staticmethod
    def acquire(db: Session, pcct_uuid: str, user_id: str):
        existing = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.is_killed == False
        ).first()

        if existing and existing.expires_at > now():
            if existing.user_id == user_id:
                return existing
            else:
                raise Exception("LOCKED_BY_OTHER")

        if existing:
            existing.is_killed = True

        lock = LockEntry(
            pcct_uuid=pcct_uuid,
            user_id=user_id,
            locked_at=now(),
            last_heartbeat=now(),
            expires_at=now() + timedelta(seconds=LOCK_TTL_SECONDS)
        )

        db.add(lock)
        db.add(LockLog(
            pcct_uuid=pcct_uuid,
            user_id=user_id,
            action="ACQUIRE"
        ))

        db.commit()
        db.refresh(lock)
        return lock

    @staticmethod
    def heartbeat(db: Session, pcct_uuid: str, user_id: str):
        lock = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.user_id == user_id
        ).first()

        if not lock:
            raise Exception("NO_LOCK")

        if lock.is_killed:
            return {"status": "KILLED"}

        if lock.expires_at < now():
            lock.is_killed = True
            db.commit()
            return {"status": "EXPIRED"}

        lock.last_heartbeat = now()
        lock.expires_at = now() + timedelta(seconds=LOCK_TTL_SECONDS)
        db.commit()

        return {"status": "OK"}

    @staticmethod
    def release(db: Session, pcct_uuid: str, user_id: str):
        lock = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.user_id == user_id,
            LockEntry.is_killed == False
        ).first()

        if not lock:
            raise Exception("NO_ACTIVE_LOCK")

        lock.is_killed = True

        db.add(LockLog(
            pcct_uuid=pcct_uuid,
            user_id=user_id,
            action="RELEASE"
        ))

        db.commit()

        @staticmethod
    def steal(db: Session, pcct_uuid: str, user_id: str):
        existing = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.is_killed == False
        ).first()

        if not existing:
            return LockService.acquire(db, pcct_uuid, user_id)

        if existing.user_id == user_id:
            return existing

        # убиваем старый lock
        existing.is_killed = True

        db.add(LockLog(
            pcct_uuid=pcct_uuid,
            user_id=user_id,
            action="STEAL"
        ))

        # создаём новый
        new_lock = LockEntry(
            pcct_uuid=pcct_uuid,
            user_id=user_id,
            locked_at=now(),
            last_heartbeat=now(),
            expires_at=now() + timedelta(seconds=LOCK_TTL_SECONDS)
        )

        db.add(new_lock)
        db.commit()
        db.refresh(new_lock)

        return new_lock
    
        @staticmethod
    def cleanup(db: Session):
        expired = db.query(LockEntry).filter(
            LockEntry.is_killed == False,
            LockEntry.expires_at < now()
        ).all()

        for lock in expired:
            lock.is_killed = True
            db.add(LockLog(
                pcct_uuid=lock.pcct_uuid,
                user_id=lock.user_id,
                action="CLEANUP"
            ))

        db.commit()

        @staticmethod
    def validate_lock(db: Session, pcct_uuid: str, user_id: str):
        lock = db.query(LockEntry).filter(
            LockEntry.pcct_uuid == pcct_uuid,
            LockEntry.user_id == user_id,
            LockEntry.is_killed == False
        ).first()

        if not lock:
            raise Exception("NO_VALID_LOCK")

        if lock.expires_at < now():
            lock.is_killed = True
            db.commit()
            raise Exception("LOCK_EXPIRED")

        return True