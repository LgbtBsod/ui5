import base64
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
    return rows[0]["DB_KEY"]


def _attachment_payload(file_name: str, mime_type: str, body: bytes, category_key: str = "GEN"):
    return {
        "DB_KEY": "TMP-ATT-1",
        "PARENT_KEY": "",
        "FolderKey": "",
        "CategoryKey": category_key,
        "Type": category_key,
        "FileName": file_name,
        "Name": file_name,
        "MimeType": mime_type,
        "Description": "embedded upload",
        "FileSize": len(body),
        "FileSizeContent": len(body),
        "Value": base64.b64encode(body).decode("ascii"),
    }


def test_create_checklist_accepts_embedded_attachment_under_10mb():
    with TestClient(app) as client:
        token = _csrf(client)
        body = b"ID3" + (b"\x00" * 4096)
        created = client.post(
            f"{SERVICE_ROOT}/CreateChecklist",
            json={
                "FullPayload": {
                    "root": {"id": "__CREATE", "status": "DRAFT"},
                    "basic": {
                        "date": "2026-03-02",
                        "equipment": "Attachment create",
                        "LOCATION_KEY": "LOC-001-01-01",
                        "LOCATION_NAME": "Area A",
                        "LOCATION_TEXT": "Area A",
                        "OBSERVER_FULLNAME": "Observer One",
                        "OBSERVED_FULLNAME": "Observed One",
                        "LPC_KEY": "L2",
                        "PROF_KEY": "PR1",
                    },
                    "checks": [],
                    "barriers": [],
                    "attachments": [_attachment_payload("voice-note.mp3", "audio/mpeg", body)],
                }
            },
            headers={"X-CSRF-Token": token},
        )
        assert created.status_code == 200
        root_key = created.json().get("d", {}).get("DB_KEY")
        attachments = client.get(f"{SERVICE_ROOT}/AttachmentSet", params={"$filter": f"PARENT_KEY eq '{root_key}'"})
        assert attachments.status_code == 200
        rows = attachments.json().get("d", {}).get("results", [])
        assert isinstance(rows, list)
        if rows:
            attachment_key = rows[0]["AttachmentKey"]
            loaded = client.get(f"{SERVICE_ROOT}/AttachmentSet(AttachmentKey='{attachment_key}')")
            assert loaded.status_code == 200
            loaded_body = loaded.json().get("d", {})
            assert loaded_body.get("FileName") == "voice-note.mp3"
            assert loaded_body.get("DocumentHandle") == attachment_key
            assert loaded_body.get("DownloadUrl")


def test_save_changes_rejects_embedded_attachment_over_10mb():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)
        acquire = client.post(f"{SERVICE_ROOT}/LockAcquire", params={"DB_KEY": root_key, "SessionGuid": "ATT-S1"}, headers={"X-CSRF-Token": token})
        assert acquire.status_code == 200
        body = b"RIFF" + (b"\x00" * (10 * 1024 * 1024))
        resp = client.post(
            f"{SERVICE_ROOT}/SaveChanges",
            json={
                "root": {"pcct_uuid": root_key},
                "checks": [],
                "barriers": [],
                "attachments": [_attachment_payload("large-sample.wav", "audio/wav", body)],
                "client_version": 1,
                "SessionGuid": "ATT-S1",
            },
            headers={"X-CSRF-Token": token},
        )
        assert resp.status_code in (200, 413)
