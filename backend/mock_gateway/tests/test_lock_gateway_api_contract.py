import os
import sys
import uuid

from fastapi.testclient import TestClient

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from database import Base, SessionLocal, engine  # noqa: E402
from main import app  # noqa: E402
from models import ChecklistRoot  # noqa: E402
from utils.odata import SERVICE_ROOT  # noqa: E402
from conftest import _csrf  # noqa: E402

def setup_function():
    Base.metadata.drop_all(bind=engine)
    Base.metadata.create_all(bind=engine)

def teardown_function():
    Base.metadata.drop_all(bind=engine)

def _create_root(root_id: str):
    with SessionLocal() as db:
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

def test_lock_heartbeat_wrong_session_returns_locked_by_other_not_expired():
    root_id = str(uuid.uuid4())
    _create_root(root_id)

    with TestClient(app) as client:
        token = _csrf(client)
        acquire = client.post(
            f"{SERVICE_ROOT}/LockAcquire",
            params={"DB_KEY": root_id, "SessionGuid": "S1"},
            headers={"X-CSRF-Token": token}
        )
        assert acquire.status_code == 200

        heartbeat = client.post(
            f"{SERVICE_ROOT}/LockHeartbeat",
            params={"DB_KEY": root_id, "SessionGuid": "S2"},
            headers={"X-CSRF-Token": token}
        )
        assert heartbeat.status_code == 200
        body = heartbeat.json().get("d", {})
        assert body.get("Code") == "LOCK_NOT_OWNED_BY_SESSION"
        assert body.get("ReasonCode") == "LOCKED_BY_OTHER"
        assert body.get("Action") == "FAILED"
        assert body.get("OwnerSession") == "S1"

def test_lock_release_wrong_session_does_not_report_free():
    root_id = str(uuid.uuid4())
    _create_root(root_id)

    with TestClient(app) as client:
        token = _csrf(client)
        acquire = client.post(
            f"{SERVICE_ROOT}/LockAcquire",
            params={"DB_KEY": root_id, "SessionGuid": "S1"},
            headers={"X-CSRF-Token": token}
        )
        assert acquire.status_code == 200

        release = client.post(
            f"{SERVICE_ROOT}/LockRelease",
            params={"DB_KEY": root_id, "SessionGuid": "S2"},
            headers={"X-CSRF-Token": token}
        )
        assert release.status_code == 200
        body = release.json().get("d", {})
        assert body.get("Success") is False
        assert body.get("Code") == "LOCK_NOT_OWNED_BY_SESSION"
        assert body.get("ReasonCode") == "LOCKED_BY_OTHER"
        assert body.get("Action") == "FAILED"
        assert body.get("OwnerSession") == "S1"

def test_copy_checklist_uses_canonical_db_key_query_parameter():
    root_id = str(uuid.uuid4())
    _create_root(root_id)

    with TestClient(app) as client:
        token = _csrf(client)
        copied = client.post(
            f"{SERVICE_ROOT}/CopyChecklist",
            params={"DB_KEY": root_id, "SessionGuid": "S1"},
            headers={"X-CSRF-Token": token}
        )
        assert copied.status_code == 200
        body = copied.json().get("d", {})
        assert body.get("Ok") is True
        assert body.get("DB_KEY")
