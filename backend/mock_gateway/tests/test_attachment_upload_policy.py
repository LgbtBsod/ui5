import os
import sys
import base64

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


def _attachment_headers(root_key: str, file_name: str, mime_type: str, category_key: str = "GEN"):
    return {
        "X-DB-Key": root_key,
        "X-Parent-Key": root_key,
        "X-Folder-Key": root_key,
        "X-Category-Key": category_key,
        "X-Description": "media upload",
        "X-File-Name": file_name,
        "Slug": file_name,
        "Content-Type": mime_type,
    }


def test_attachment_create_accepts_media_upload_under_10mb():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)
        body = b"ID3" + (b"\x00" * 4096)
        created = client.post(
            f"{SERVICE_ROOT}/AttachmentSet",
            content=body,
            headers=dict(_attachment_headers(root_key, "voice-note.mp3", "audio/mpeg"), **{"X-CSRF-Token": token}),
        )
        assert created.status_code == 200
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


def test_attachment_create_rejects_media_upload_over_10mb():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)
        body = b"RIFF" + (b"\x00" * (10 * 1024 * 1024))
        resp = client.post(
            f"{SERVICE_ROOT}/AttachmentSet",
            content=body,
            headers=dict(_attachment_headers(root_key, "large-sample.wav", "audio/wav"), **{"X-CSRF-Token": token}),
        )
        assert resp.status_code == 413


def test_save_changes_rejects_base64_attachment_payloads():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)
        acquire = client.post(
            f"{SERVICE_ROOT}/LockAcquire",
            json={"DB_KEY": root_key, "SessionGuid": "SESSION-ATTACH-SAVE"},
            headers={"X-CSRF-Token": token},
        )
        assert acquire.status_code == 200

        payload = {
            "Payload": {
                "root": {
                    "pcct_uuid": root_key,
                    "edit_mode": "U",
                },
                "checks": [],
                "barriers": [],
                "participants": [],
                "attachments": [{
                    "AttachmentKey": "A-1",
                    "FileName": "legacy.txt",
                    "MimeType": "text/plain",
                    "ContentBase64": base64.b64encode(b"legacy").decode("ascii"),
                    "edit_mode": "U",
                }],
                "session_guid": "SESSION-ATTACH-SAVE",
                "client_version": 1,
            }
        }
        resp = client.post(
            f"{SERVICE_ROOT}/SaveChanges",
            json=payload,
            headers={"X-CSRF-Token": token},
        )
        assert resp.status_code == 400
