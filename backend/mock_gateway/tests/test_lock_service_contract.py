import uuid

from sqlalchemy.exc import IntegrityError

from config import LOCK_KILLED_RETENTION, LOCK_TTL
from database import SessionLocal
from models import ChecklistRoot, LockEntry
from services import checklist_service
from services.lock_service import LockService


def _create_root(db, root_id: str):
    root = ChecklistRoot(
        id=root_id,
        checklist_id="CHK-" + root_id[-6:],
        lpc="L1",
        status="01",
        created_by="TEST",
        changed_by="TEST",
    )
    db.add(root)
    db.commit()
    return root


def test_acquire_handles_integrity_race(monkeypatch):
    with SessionLocal() as db:
        root_id = str(uuid.uuid4())
        _create_root(db, root_id)

        original_create = LockService._create_active_lock
        calls = {"count": 0}

        def fake_create(session, object_uuid, session_guid, uname, current_time):
            calls["count"] += 1
            if calls["count"] == 1:
                competing = LockEntry(
                    pcct_uuid=object_uuid,
                    user_id="OTHER",
                    session_guid="OTHER-S",
                    locked_at=current_time,
                    last_heartbeat=current_time,
                    expires_at=current_time + LOCK_TTL,
                )
                session.add(competing)
                session.commit()
                raise IntegrityError("insert", {}, None)
            return original_create(session, object_uuid, session_guid, uname, current_time)

        monkeypatch.setattr(LockService, "_create_active_lock", staticmethod(fake_create))

        result = LockService.acquire(db, root_id, "S1", "USER1")

        assert result["success"] is False
        assert result["action"] == "LOCKED"
        assert result["owner"] == "OTHER"


def test_release_try_save_reports_save_status(monkeypatch):
    with SessionLocal() as db:
        root_id = str(uuid.uuid4())
        _create_root(db, root_id)
        LockService.acquire(db, root_id, "S1", "USER1")

        class FakeChecklistService:
            @staticmethod
            def save_via_import(_db, _root_id, _user_id, _payload, is_autosave=False, force=False, session_guid=None):
                assert is_autosave is False
                assert force is True
                assert session_guid == "S1"
                return {"ok": True}

        monkeypatch.setattr(checklist_service, "ChecklistService", FakeChecklistService)

        result = LockService.release(db, root_id, "S1", try_save=True, payload={"basic": {"Status": "02"}})

        assert result["released"] is True
        assert result["save_status"] == "S"


def test_killed_lock_retention_exceeds_ttl():
    assert LOCK_KILLED_RETENTION > LOCK_TTL
