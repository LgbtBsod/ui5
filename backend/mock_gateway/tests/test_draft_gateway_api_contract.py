import os
import sys
import uuid

from fastapi.testclient import TestClient

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from database import Base, SessionLocal, engine  # noqa: E402
from main import app  # noqa: E402
from models import ChecklistRoot, ChecklistRootDraft  # noqa: E402
from utils.odata import SERVICE_ROOT  # noqa: E402
from conftest import _csrf  # noqa: E402


def setup_function():
    Base.metadata.drop_all(bind=engine)
    Base.metadata.create_all(bind=engine)


def teardown_function():
    Base.metadata.drop_all(bind=engine)


def _create_active_root(root_id: str):
    with SessionLocal() as db:
        root = ChecklistRoot(
            id=root_id,
            checklist_id="CHK-" + root_id[-6:],
            lpc="L1",
            status="01",
            created_by="TEST",
            changed_by="TEST",
            version_number=1,
        )
        db.add(root)
        db.commit()


def _draft_count() -> int:
    with SessionLocal() as db:
        return db.query(ChecklistRootDraft).count()


def test_create_draft_then_activate():
    with TestClient(app) as client:
        token = _csrf(client)

        prep = client.post(
            f"{SERVICE_ROOT}/ChecklistRootPreparationAction",
            headers={"X-CSRF-Token": token},
        )
        assert prep.status_code == 200
        draft_body = prep.json()["d"]
        assert draft_body["IsActiveEntity"] is False
        assert draft_body["HasActiveEntity"] is False
        draft_uuid = draft_body["DraftUUID"]
        assert draft_uuid

        assert _draft_count() == 1

        activate = client.post(
            f"{SERVICE_ROOT}/ChecklistRootActivationAction",
            params={"DraftUUID": draft_uuid},
            headers={"X-CSRF-Token": token},
        )
        assert activate.status_code == 200
        active_body = activate.json()["d"]
        assert active_body["IsActiveEntity"] is True
        assert active_body["HasDraftEntity"] is False
        hex_key = active_body["DB_KEY"]

        assert _draft_count() == 0

        read = client.get(f"{SERVICE_ROOT}/ChecklistRootSet('{hex_key}')")
        assert read.status_code == 200
        assert read.json()["d"]["DB_KEY"] == hex_key


def test_create_draft_then_discard_returns_204_and_no_active_row():
    with TestClient(app) as client:
        token = _csrf(client)

        prep = client.post(
            f"{SERVICE_ROOT}/ChecklistRootPreparationAction",
            headers={"X-CSRF-Token": token},
        )
        assert prep.status_code == 200
        draft_uuid = prep.json()["d"]["DraftUUID"]

        discard = client.post(
            f"{SERVICE_ROOT}/ChecklistRootDiscardAction",
            params={"DraftUUID": draft_uuid},
            headers={"X-CSRF-Token": token},
        )
        assert discard.status_code == 204
        assert _draft_count() == 0

        with SessionLocal() as db:
            assert db.query(ChecklistRoot).count() == 0


def test_edit_draft_idempotent_prepare_then_activate():
    root_id = str(uuid.uuid4())
    _create_active_root(root_id)

    with TestClient(app) as client:
        token = _csrf(client)

        prep1 = client.post(
            f"{SERVICE_ROOT}/ChecklistRootPreparationAction",
            params={"ActiveUUID": root_id},
            headers={"X-CSRF-Token": token},
        )
        assert prep1.status_code == 200
        draft1 = prep1.json()["d"]

        prep2 = client.post(
            f"{SERVICE_ROOT}/ChecklistRootPreparationAction",
            params={"ActiveUUID": root_id},
            headers={"X-CSRF-Token": token},
        )
        assert prep2.status_code == 200
        draft2 = prep2.json()["d"]

        assert draft1["DraftUUID"] == draft2["DraftUUID"]
        assert _draft_count() == 1

        active_hex = draft1["ActiveUUID"]
        draft_uuid = draft1["DraftUUID"]

        patch = client.patch(
            f"{SERVICE_ROOT}/ChecklistRootSet(ActiveUUID='{active_hex}',DraftUUID='{draft_uuid}')",
            json={"Status": "REGISTERED"},
            headers={"X-CSRF-Token": token},
        )
        assert patch.status_code == 200

        unchanged_read = client.get(f"{SERVICE_ROOT}/ChecklistRootSet('{active_hex}')")
        assert unchanged_read.json()["d"]["Status"] == "DRAFT"

        activate = client.post(
            f"{SERVICE_ROOT}/ChecklistRootActivationAction",
            params={"DraftUUID": draft_uuid},
            headers={"X-CSRF-Token": token},
        )
        assert activate.status_code == 200
        activated = activate.json()["d"]
        assert activated["Status"] == "REGISTERED"
        assert activated["VersionNumber"] == 2
        assert _draft_count() == 0


def test_edit_draft_discard_keeps_active_untouched():
    root_id = str(uuid.uuid4())
    _create_active_root(root_id)

    with TestClient(app) as client:
        token = _csrf(client)

        prep = client.post(
            f"{SERVICE_ROOT}/ChecklistRootPreparationAction",
            params={"ActiveUUID": root_id},
            headers={"X-CSRF-Token": token},
        )
        draft = prep.json()["d"]
        active_hex = draft["ActiveUUID"]
        draft_uuid = draft["DraftUUID"]

        patch = client.patch(
            f"{SERVICE_ROOT}/ChecklistRootSet(ActiveUUID='{active_hex}',DraftUUID='{draft_uuid}')",
            json={"Status": "REGISTERED"},
            headers={"X-CSRF-Token": token},
        )
        assert patch.status_code == 200

        discard = client.post(
            f"{SERVICE_ROOT}/ChecklistRootDiscardAction",
            params={"DraftUUID": draft_uuid},
            headers={"X-CSRF-Token": token},
        )
        assert discard.status_code == 200
        assert discard.json()["d"]["Status"] == "DRAFT"
        assert _draft_count() == 0

        read = client.get(f"{SERVICE_ROOT}/ChecklistRootSet('{active_hex}')")
        assert read.json()["d"]["Status"] == "DRAFT"


def test_metadata_contains_draft_annotations_and_pre_existing_entries():
    with TestClient(app) as client:
        response = client.get(f"{SERVICE_ROOT}/$metadata")
        assert response.status_code == 200
        body = response.text
        for token in (
            "ChecklistRootPreparationAction",
            "ChecklistRootActivationAction",
            "ChecklistRootDiscardAction",
            "ActiveUUID",
            "DraftUUID",
            "IsActiveEntity",
            "LockAcquire",
            "SaveChanges",
        ):
            assert token in body
