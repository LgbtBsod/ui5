from datetime import timezone
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import Session

from config import LOCK_KILLED_RETENTION, LOCK_TTL
from models import ChecklistRoot, LockEntry, LockLog
from utils.time import now_utc


def _as_utc(dt):
    if dt is None:
        return None
    if dt.tzinfo is None:
        return dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


class LockService:
    @staticmethod
    def _lock_expires_at(lock: LockEntry | None):
        last_refresh_at = _as_utc(lock.last_refresh_at) if lock and lock.last_refresh_at else None
        return last_refresh_at + LOCK_TTL if last_refresh_at else None

    @staticmethod
    def _is_expired(lock: LockEntry | None, current_time=None) -> bool:
        current_time = current_time or now_utc()
        lock_expires_at = LockService._lock_expires_at(lock)
        return bool(lock_expires_at and lock_expires_at <= current_time)

    @staticmethod
    def _lock_payload(success: bool, code: str, owner: str, owner_session: str, lock_expires, is_killed_flag: bool = False, lock_refreshed: bool = False, owner_session_match: bool | None = None) -> dict:
        return {
            "ok": bool(success),
            "success": bool(success),
            "code": str(code or "").strip(),
            "action": str(code or "").strip(),
            "owner": str(owner or "").strip(),
            "owner_session": str(owner_session or "").strip(),
            "owner_session_match": owner_session_match,
            "lock_expires": lock_expires,
            "lock_expires_at": lock_expires,
            "server_now": now_utc(),
            "lock_refreshed": bool(lock_refreshed),
            "is_killed_flag": bool(is_killed_flag),
            "server_changed_on": None,
            "version_number": 0,
        }

    @staticmethod
    def _with_server_state(payload: dict, db: Session, object_uuid: str) -> dict:
        payload["server_changed_on"] = LockService._server_changed_on(db, object_uuid)
        payload["version_number"] = LockService._version_number(db, object_uuid)
        return payload

    @staticmethod
    def _create_active_lock(db: Session, object_uuid: str, session_guid: str, uname: str, current_time) -> LockEntry:
        lock = LockEntry(
            pcct_uuid=object_uuid,
            user_id=uname,
            session_guid=session_guid,
            locked_at=current_time,
            last_refresh_at=current_time,
            expires_at=current_time + LOCK_TTL,
        )
        db.add(lock)
        db.flush()
        db.add(LockLog(pcct_uuid=object_uuid, user_id=uname, session_guid=session_guid, action="ACQUIRED"))
        db.commit()
        db.refresh(lock)
        return lock

    @staticmethod
    def _active_lock(db: Session, object_uuid: str) -> LockEntry | None:
        current_time = now_utc()
        lock = (
            db.query(LockEntry)
            .filter(
                LockEntry.pcct_uuid == object_uuid,
                LockEntry.is_killed.is_(False),
            )
            .first()
        )
        if not lock:
            return None

        if LockService._is_expired(lock, current_time):
            lock.is_killed = True
            db.add(LockLog(pcct_uuid=object_uuid, user_id=lock.user_id, action="EXPIRED"))
            db.commit()
            return None
        return lock

    @staticmethod
    def acquire(db: Session, object_uuid: str, session_guid: str, uname: str, steal_from: str | None = None) -> dict:
        current_time = now_utc()
        existing = LockService._active_lock(db, object_uuid)

        if not existing:
            try:
                lock = LockService._create_active_lock(db, object_uuid, session_guid, uname, current_time)
                return LockService._with_server_state(
                    LockService._lock_payload(True, "LOCK_OK", uname, session_guid, LockService._lock_expires_at(lock), False, True, True),
                    db,
                    object_uuid
                )
            except IntegrityError:
                db.rollback()
                existing = LockService._active_lock(db, object_uuid)
                if not existing:
                    raise

        if existing.session_guid == session_guid:
            existing.last_refresh_at = current_time
            existing.expires_at = current_time + LOCK_TTL
            db.add(LockLog(pcct_uuid=object_uuid, user_id=uname, session_guid=session_guid, action="REFRESHED"))
            db.commit()
            return LockService._with_server_state(
                LockService._lock_payload(True, "LOCK_OK", existing.user_id, existing.session_guid, LockService._lock_expires_at(existing), False, True, True),
                db,
                object_uuid
            )

        if existing.user_id == uname and steal_from and steal_from == existing.session_guid:
            existing.is_killed = True
            existing.killed_by = session_guid

            new_lock = LockEntry(
                pcct_uuid=object_uuid,
                user_id=uname,
                session_guid=session_guid,
                locked_at=current_time,
                last_refresh_at=current_time,
                expires_at=current_time + LOCK_TTL,
            )
            db.add(new_lock)
            db.add(LockLog(pcct_uuid=object_uuid, user_id=uname, session_guid=session_guid, action="STEAL_OWN_SESSION"))
            db.commit()
            return LockService._with_server_state(
                LockService._lock_payload(True, "LOCK_OK", uname, session_guid, LockService._lock_expires_at(new_lock), True, True, True),
                db,
                object_uuid
            )

        return LockService._with_server_state(
            LockService._lock_payload(False, "LOCK_NOT_OWNED_BY_SESSION", existing.user_id, existing.session_guid, LockService._lock_expires_at(existing), False, False, False),
            db,
            object_uuid
        )

    @staticmethod
    def status(db: Session, object_uuid: str, session_guid: str) -> dict:
        active_lock = LockService._active_lock(db, object_uuid)
        if not active_lock:
            raise ValueError("LOCK_MISSING")
        if str(active_lock.session_guid or "").strip() != str(session_guid or "").strip():
            return LockService._with_server_state(
                LockService._lock_payload(False, "LOCK_NOT_OWNED_BY_SESSION", active_lock.user_id, active_lock.session_guid, LockService._lock_expires_at(active_lock), False, False, False),
                db,
                object_uuid
            )
        return LockService._with_server_state(
            LockService._lock_payload(True, "LOCK_OK", active_lock.user_id, active_lock.session_guid, LockService._lock_expires_at(active_lock), False, False, True),
            db,
            object_uuid
        )

    @staticmethod
    def heartbeat(db: Session, object_uuid: str, session_guid: str) -> dict:
        active_lock = LockService._active_lock(db, object_uuid)
        if not active_lock:
            raise ValueError("LOCK_MISSING")
        if str(active_lock.session_guid or "").strip() != str(session_guid or "").strip():
            raise ValueError("LOCK_NOT_OWNED_BY_SESSION")

        active_lock.last_refresh_at = now_utc()
        active_lock.expires_at = active_lock.last_refresh_at + LOCK_TTL
        db.commit()

        return LockService._with_server_state(
            LockService._lock_payload(True, "LOCK_OK", active_lock.user_id, active_lock.session_guid, LockService._lock_expires_at(active_lock), False, True, True),
            db,
            object_uuid
        )

    @staticmethod
    def release(db: Session, object_uuid: str, session_guid: str, try_save: bool = False, payload: dict | None = None) -> dict:
        lock = (
            db.query(LockEntry)
            .filter(
                LockEntry.pcct_uuid == object_uuid,
                LockEntry.session_guid == session_guid,
                LockEntry.is_killed.is_(False),
            )
            .first()
        )

        if not lock:
            return {"released": False, "save_status": "N", "ok": False, "code": "LOCK_MISSING"}

        s_save_status = "N"
        if try_save and isinstance(payload, dict):
            try:
                from services.checklist_service import ChecklistService
                ChecklistService.save_via_import(db, object_uuid, lock.user_id, payload, is_autosave=False, force=True, session_guid=session_guid)
                s_save_status = "S"
            except Exception:
                db.rollback()
                s_save_status = "E"

        lock.is_killed = True
        db.add(LockLog(pcct_uuid=object_uuid, user_id=lock.user_id, session_guid=session_guid, action="RELEASE"))
        db.commit()
        return {"released": True, "save_status": s_save_status, "ok": True, "code": "LOCK_OK"}

    @staticmethod
    def cleanup(db: Session) -> int:
        current_time = now_utc()
        expired = (
            db.query(LockEntry)
            .filter(
                LockEntry.is_killed.is_(False),
                LockEntry.last_refresh_at < (current_time - LOCK_TTL),
            )
            .all()
        )
        killed_old = (
            db.query(LockEntry)
            .filter(
                LockEntry.is_killed.is_(True),
                LockEntry.last_refresh_at < (current_time - LOCK_KILLED_RETENTION),
            )
            .all()
        )

        for lock in expired:
            lock.is_killed = True
            db.add(LockLog(pcct_uuid=lock.pcct_uuid, user_id=lock.user_id, session_guid=lock.session_guid, action="CLEANUP_EXPIRED"))

        for lock in killed_old:
            db.delete(lock)

        db.commit()
        return len(expired) + len(killed_old)

    @staticmethod
    def validate_lock(db: Session, pcct_uuid: str, user_id: str) -> bool:
        lock = (
            db.query(LockEntry)
            .filter(
                LockEntry.pcct_uuid == pcct_uuid,
                LockEntry.user_id == user_id,
                LockEntry.is_killed.is_(False),
            )
            .first()
        )

        if not lock:
            raise ValueError("LOCK_MISSING")

        if LockService._is_expired(lock):
            lock.is_killed = True
            db.commit()
            raise ValueError("LOCK_EXPIRED")

        return True

    @staticmethod
    def validate_session_lock(db: Session, pcct_uuid: str, session_guid: str) -> bool:
        lock = LockService._active_lock(db, pcct_uuid)
        if not lock:
            raise ValueError("LOCK_MISSING")
        if str(lock.session_guid or "").strip() != str(session_guid or "").strip():
            raise ValueError("LOCK_NOT_OWNED_BY_SESSION")
        return True

    @staticmethod
    def _server_changed_on(db: Session, object_uuid: str):
        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == object_uuid).first()
        return root.changed_on if root else None

    @staticmethod
    def _version_number(db: Session, object_uuid: str):
        ts = LockService._server_changed_on(db, object_uuid)
        return int(ts.timestamp()) if ts else 0
