import io
import os
import sys

from fastapi.testclient import TestClient

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from main import app  # noqa: E402

SERVICE_ROOT = "/sap/opu/odata/sap/Z_UI5_SRV"


def _csrf(client: TestClient):
    resp = client.get(f"{SERVICE_ROOT}/", headers={"X-CSRF-Token": "Fetch"})
    return resp.headers.get("X-CSRF-Token")


def _sample_root(client: TestClient):
    payload = client.get(f"{SERVICE_ROOT}/ChecklistSearchSet", params={"$top": 1}).json()
    rows = payload.get("d", {}).get("results", [])
    assert rows
    return rows[0]["Key"]


def test_attachment_upload_accepts_audio_under_10mb():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)
        body = b"ID3" + (b"\x00" * 4096)
        created = client.post(
            f"{SERVICE_ROOT}/AttachmentSet",
            json={
                "RootKey": root_key,
                "FolderKey": root_key,
                "CategoryKey": "GEN",
                "FileName": "voice-note.mp3",
                "MimeType": "audio/mpeg",
                "FileSize": len(body),
                "ClientRowId": "TMP-ATT-1",
            },
            headers={"X-CSRF-Token": token},
        )
        assert created.status_code == 201
        attachment_key = created.json().get("d", {}).get("AttachmentKey")
        assert attachment_key

        resp = client.put(
            f"{SERVICE_ROOT}/AttachmentSet(Key='{attachment_key}')/$value",
            content=body,
            headers={
                "X-CSRF-Token": token,
                "Content-Type": "audio/mpeg",
                "Slug": "voice-note.mp3",
                "X-RootKey": root_key,
                "X-CategoryKey": "GEN",
            },
        )

        assert resp.status_code == 204

        downloaded = client.get(
            f"{SERVICE_ROOT}/AttachmentSet(Key='{attachment_key}')/$value",
            headers={"X-CSRF-Token": token},
        )

        assert downloaded.status_code == 200
        assert downloaded.content == body


def test_attachment_upload_rejects_payload_over_10mb():
    with TestClient(app) as client:
        token = _csrf(client)
        root_key = _sample_root(client)
        body = io.BytesIO()
        body.write(b"RIFF")
        body.write(b"\x00" * (10 * 1024 * 1024))
        created = client.post(
            f"{SERVICE_ROOT}/AttachmentSet",
            json={
                "RootKey": root_key,
                "FolderKey": root_key,
                "CategoryKey": "GEN",
                "FileName": "large-sample.wav",
                "MimeType": "audio/wav",
                "FileSize": len(body.getvalue()),
                "ClientRowId": "TMP-ATT-2",
            },
            headers={"X-CSRF-Token": token},
        )
        assert created.status_code == 201
        attachment_key = created.json().get("d", {}).get("AttachmentKey")
        assert attachment_key

        resp = client.put(
            f"{SERVICE_ROOT}/AttachmentSet(Key='{attachment_key}')/$value",
            content=body.getvalue(),
            headers={
                "X-CSRF-Token": token,
                "Content-Type": "audio/wav",
                "Slug": "large-sample.wav",
                "X-RootKey": root_key,
                "X-CategoryKey": "GEN",
            },
        )

        assert resp.status_code == 413
