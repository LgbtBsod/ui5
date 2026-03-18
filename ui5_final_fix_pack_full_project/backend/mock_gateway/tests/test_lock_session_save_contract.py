import os
import sys

from fastapi.testclient import TestClient

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from main import app  # noqa: E402
from utils.odata import SERVICE_ROOT  # noqa: E402


def _csrf(client: TestClient):
    resp = client.get(f"{SERVICE_ROOT}/", headers={"X-CSRF-Token": "Fetch"})
    return resp.headers.get("X-CSRF-Token")


def _sample_root(client: TestClient):
    payload = client.get(f"{SERVICE_ROOT}/ChecklistSearchSet", params={"$top": 1}).json()
    rows = payload.get("d", {}).get("results", [])
    assert rows
    return rows[0]["Key"]


def test_save_changes_requires_active_session_lock_but_not_client_version():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)

        acquire = client.post(
            f"{SERVICE_ROOT}/LockAcquire",
            params={"RootId": root_key, "SessionGuid": "SAVE-S1"},
            headers={"X-CSRF-Token": token}
        )
        assert acquire.status_code == 200

        save_resp = client.post(
            f"{SERVICE_ROOT}/SaveChanges",
            json={
                "root": {"pcct_uuid": root_key},
                "checks": [],
                "barriers": [],
                "SessionGuid": "SAVE-S1"
            },
            headers={"X-CSRF-Token": token}
        )
        assert save_resp.status_code == 200
        body = save_resp.json().get("d", {})
        assert body.get("code") == "LOCK_OK"
        assert body.get("lock_refreshed") is True
        assert body.get("lock_expires_at")
        assert body.get("server_now")
        assert body.get("request_id")
        assert int(body.get("version_number") or 0) >= 1


def test_autosave_requires_active_session_lock_but_not_client_version():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)

        copied = client.post(
            f"{SERVICE_ROOT}/CopyChecklist",
            params={"RootId": root_key, "SessionGuid": "AUTO-S1"},
            headers={"X-CSRF-Token": token}
        )
        assert copied.status_code == 200
        copied_key = copied.json().get("d", {}).get("RootKey")
        assert copied_key

        autosave_resp = client.post(
            f"{SERVICE_ROOT}/AutoSave",
            json={
                "root": {"pcct_uuid": copied_key},
                "checks": [],
                "barriers": [],
                "SessionGuid": "AUTO-S1"
            },
            headers={"X-CSRF-Token": token}
        )
        assert autosave_resp.status_code == 200
        body = autosave_resp.json().get("d", {})
        assert body.get("code") == "LOCK_OK"
        assert body.get("lock_refreshed") is True
        assert body.get("lock_expires_at")
        assert body.get("server_now")
        assert body.get("request_id")
        assert int(body.get("version_number") or 0) >= 1
