"""
Business logic operations: save, update, delete, attachment handling.
Data mutations, validation, persistence.
"""
import base64
import uuid
from pathlib import Path
from datetime import datetime, timezone
from sqlalchemy.orm import Session

from models import AttachmentEntry, ChecklistBarrier, ChecklistCheck, ChecklistRoot, DictionaryItem
from utils.time import now_utc
from utils.common_helpers import get_dict_text, load_upload_policy, parse_date_ymd
from api.gateway_validators import (
    _normalize_status_input, _pick_text, _pick_bool, _pick_first_present, _coerce_int,
    _normalize_child_rows, _date_ymd_from_any
)

# Re-export _hex for internal use (single source of truth in gateway_serializers)
def _hex(raw: str) -> str:
    """Convert UUID to 32-char hex string."""
    return str(raw or "").replace("-", "").upper()


def _normalize_hex_key(value: str | None) -> str:
    """Normalize a UUID/hex key to the canonical 32-char uppercase hex form produced by _hex().

    Returns "" for empty input. Raises ValueError if a non-empty value is not a valid
    UUID/hex key (32 hex chars once dashes are stripped).
    """
    raw = str(value or "").strip()
    if not raw:
        return ""
    cleaned = raw.replace("-", "").upper()
    if len(cleaned) != 32 or any(c not in "0123456789ABCDEF" for c in cleaned):
        raise ValueError(f"Invalid hex key: {value!r}")
    return cleaned

_UPLOAD_DIR = Path(__file__).resolve().parents[1] / "uploads"
_UPLOAD_DIR.mkdir(parents=True, exist_ok=True)


def _normalize_upload_list(value) -> list:
    items = value if isinstance(value, list) else []
    return [str(item).strip().lower() for item in items if item]


def _resolve_upload_mime(file_name: str, declared_mime: str) -> str:
    resolved = mimetypes.guess_type(str(file_name or ""))[0]
    return (resolved or str(declared_mime or "")).strip() or "application/octet-stream"


def _dict_text(db: Session, domain: str, key: str) -> str:
    from models import DictionaryItem
    item = db.query(DictionaryItem).filter(DictionaryItem.domain == domain, DictionaryItem.key == key).first()
    return item.text if item else ""


def _validate_attachment_upload(db: Session, file_name: str, content_type: str, file_size: int):
    """
    Validate attachment upload against configured upload policy.

    Checks file size, MIME type, and extension against runtime settings.
    Upload policy is stored in runtime-settings table and can be dynamically updated
    without code changes.

    Args:
        db: Database session
        file_name: Filename with extension (e.g., 'report.pdf')
        content_type: MIME type from request (e.g., 'application/pdf')
        file_size: File size in bytes

    Returns:
        (resolved_mime_type: str, error: Optional[ODataError])
        - On success: (mime_type, None)
        - On policy violation: (None, error_response)

    Raises:
        Returns error tuple on violation (not exception):
        - 413 Payload Too Large (exceeds maxSizeMb)
        - 415 Unsupported Media Type (extension or MIME not allowed)
    """
    policy = load_upload_policy(db)
    allowed_mime = set(_normalize_upload_list(policy.get("allowedMime")))
    allowed_extensions = set(_normalize_upload_list(policy.get("allowedExtensions")))
    resolved_mime = _resolve_upload_mime(file_name, content_type)
    extension = Path(str(file_name or "")).suffix.lstrip(".").lower()
    max_size_mb = 0
    try:
        max_size_mb = float(policy.get("maxSizeMb") or 0)
    except (ValueError, TypeError):
        max_size_mb = 0
    if max_size_mb > 0 and int(file_size or 0) > int(max_size_mb * 1024 * 1024):
        from utils.odata import odata_error_response
        return None, odata_error_response(413, "UPLOAD_POLICY_REJECTED", "Attachment exceeds allowed size")
    if allowed_extensions and extension and extension not in allowed_extensions:
        from utils.odata import odata_error_response
        return None, odata_error_response(415, "UPLOAD_POLICY_REJECTED", "Attachment file extension is not allowed")
    if allowed_mime and resolved_mime not in allowed_mime:
        from utils.odata import odata_error_response
        return None, odata_error_response(415, "UPLOAD_POLICY_REJECTED", "Attachment mime type is not allowed")
    return resolved_mime, None


def _normalize_attachment_key(value: str) -> str:
    raw = str(value or "").strip()
    if not raw:
        return str(uuid.uuid4())
    try:
        return str(uuid.UUID(raw))
    except ValueError:
        pass
    cleaned = raw.replace("-", "")
    if len(cleaned) == 32:
        try:
            return str(uuid.UUID(hex=cleaned))
        except ValueError:
            pass
    return str(uuid.uuid4())


def _normalize_attachment_category_key(db: Session, category_key: str) -> str:
    resolved = str(category_key or "GEN").strip() or "GEN"
    category_item = db.query(DictionaryItem).filter(DictionaryItem.domain == "ATF_CAT", DictionaryItem.key == resolved).first()
    return resolved if category_item else "GEN"


def _normalize_attachment_payload_rows(items, root_hex: str) -> list[dict]:
    rows = items if isinstance(items, list) else []
    normalized = []
    for row in rows:
        item = row if isinstance(row, dict) else {}
        normalized.append({
            "AttachmentKey": _pick_text(item, "AttachmentKey", "Key", "key"),
            "DB_KEY": _pick_text(item, "DB_KEY", "Key", "key"),
            "EditMode": _pick_text(item, "edit_mode", "EditMode"),
            "PARENT_KEY": root_hex,
            "FolderKey": _pick_text(item, "FolderKey", "folder_key") or root_hex,
            "CategoryKey": _pick_text(item, "CategoryKey", "category_key", "Type", "type") or "GEN",
            "FileName": _pick_text(item, "FileName", "file_name", "Name", "name"),
            "MimeType": _pick_text(item, "MimeType", "mime_type", "Mimetype", "mimtype") or "application/octet-stream",
            "Description": _pick_text(item, "Description", "description", "Desc", "desc"),
            "FileSize": _coerce_int(_pick_first_present(item, "FileSize", "file_size", "FileSizeContent", "filesize_content"), 0),
            "ContentBase64": _pick_text(item, "ContentBase64", "content_base64", "_fileBase64"),
            "MediaContent": _pick_first_present(item, "_media_content"),
            "DocumentHandle": _pick_text(item, "DocumentHandle", "document_handle"),
            "DownloadUrl": _pick_text(item, "DownloadUrl", "download_url"),
        })
    return normalized


def _media_upload_payload(
    *,
    db_key: str,
    parent_key: str,
    folder_key: str,
    category_key: str,
    file_name: str,
    mime_type: str,
    description: str,
    body: bytes,
    attachment_key: str,
) -> dict:
    """Build an attachment payload row for a raw binary media-stream upload (X-* headers + body)."""
    return {
        "DB_KEY": db_key,
        "PARENT_KEY": parent_key,
        "FolderKey": folder_key,
        "CategoryKey": category_key,
        "FileName": file_name,
        "MimeType": mime_type,
        "Description": description,
        "AttachmentKey": attachment_key,
        "_media_content": body,
    }


def _persist_attachment_media(db: Session, root: ChecklistRoot, row: dict, media_content: bytes) -> AttachmentEntry:
    """
    Persist attachment file to disk and database.

    Workflow:
    1. Validate attachment against upload policy
    2. Write media content to _UPLOAD_DIR / {attachment_id}
    3. Create database record (AttachmentEntry) with metadata
    4. Return created entry (db.add() called, db.commit() caller's responsibility)

    Args:
        db: Database session
        root: Parent ChecklistRoot entity
        row: Attachment metadata dict (FileName, MimeType, CategoryKey, Description, etc.)
        media_content: Raw file bytes

    Returns:
        Newly created AttachmentEntry (not yet committed)

    Raises:
        ValueError: If FileName missing or media_content invalid
        RuntimeError: If upload policy validation fails
        OSError: If file write fails
    """
    file_name = str(row.get("FileName") or "").strip()
    if not file_name or not isinstance(media_content, (bytes, bytearray)) or not media_content:
        raise ValueError("INVALID_ATTACHMENT_PAYLOAD")
    resolved_mime_type, validation_error = _validate_attachment_upload(db, file_name, row.get("MimeType") or "", len(media_content))
    if validation_error:
        # Carry the already-built error Response through the exception (not str(), which would
        # just stringify the object's repr and silently discard the real status/message).
        raise RuntimeError(validation_error)

    attachment_id = _normalize_attachment_key(row.get("AttachmentKey") or "")
    file_path = _UPLOAD_DIR / attachment_id
    file_path.write_bytes(bytes(media_content))
    now = now_utc()
    entry = AttachmentEntry(
        id=attachment_id,
        root_id=root.id,
        folder_key=str(row.get("FolderKey") or _hex(root.id)).strip() or _hex(root.id),
        category_key=_normalize_attachment_category_key(db, row.get("CategoryKey") or "GEN"),
        file_name=file_name,
        mime_type=resolved_mime_type,
        file_size=len(media_content),
        storage_path=str(file_path),
        created_on=now,
        changed_on=now,
    )
    db.add(entry)
    return entry


def _apply_attachment_metadata(entry: AttachmentEntry, row: dict, db: Session) -> None:
    if row.get("Description") is not None:
        entry.description = str(row.get("Description") or "").strip()
    if row.get("CategoryKey"):
        entry.category_key = _normalize_attachment_category_key(db, row.get("CategoryKey"))
    if row.get("FileName"):
        entry.file_name = str(row.get("FileName") or entry.file_name or "").strip()
    entry.changed_on = now_utc()


def _apply_save_attachments(db: Session, root: ChecklistRoot, items, allow_media_content: bool = False) -> None:
    rows = _normalize_attachment_payload_rows(items, _hex(root.id))
    if not rows:
        return
    for row in rows:
        s_mode = str(row.get("EditMode") or "U").strip().upper() or "U"
        attachment_id = _normalize_attachment_key(row.get("AttachmentKey") or "")
        entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == attachment_id, AttachmentEntry.root_id == root.id).first() if row.get("AttachmentKey") else None
        media_content = row.get("MediaContent")
        if row.get("ContentBase64"):
            raise ValueError("ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN")

        if s_mode == "D":
            if entry:
                path = Path(entry.storage_path or "")
                if path.exists():
                    path.unlink()
                db.delete(entry)
            continue

        if allow_media_content and media_content:
            _persist_attachment_media(db, root, row, media_content)
            continue

        if entry:
            _apply_attachment_metadata(entry, row, db)


def _apply_root_payload(root: ChecklistRoot, root_payload: dict | None) -> None:
    data = root_payload if isinstance(root_payload, dict) else {}
    status = _pick_text(data, "Status", "status")
    lpc = _pick_text(data, "Lpc", "LPC_KEY", "lpc")
    request_id = _pick_text(data, "RequestId", "Id", "checklist_id")

    if status:
        root.status = _normalize_status_input(status)
    if lpc:
        root.lpc = lpc
    if request_id:
        root.checklist_id = request_id


def _replace_detail_rows(db: Session, root: ChecklistRoot, checks, barriers) -> None:
    db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root.id).delete()
    db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root.id).delete()

    for check in _normalize_child_rows(checks, "ChecksNum"):
        db.add(ChecklistCheck(
            id=str(uuid.uuid4()),
            root_id=root.id,
            text=check.get("Text", ""),
            comment=check.get("Comment", ""),
            status="PASS" if check.get("Result", True) else "FAIL",
            position=int(check.get("ChecksNum", 0))
        ))

    for barrier in _normalize_child_rows(barriers, "BarriersNum"):
        db.add(ChecklistBarrier(
            id=str(uuid.uuid4()),
            root_id=root.id,
            description=barrier.get("Text", ""),
            comment=barrier.get("Comment", ""),
            is_active=bool(barrier.get("Result", True)),
            position=int(barrier.get("BarriersNum", 0))
        ))


# Import at module end to avoid circular dependencies
import mimetypes
