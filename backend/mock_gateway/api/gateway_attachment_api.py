"""AttachmentFolderSet / AttachmentSet routes."""
import json
from pathlib import Path

from fastapi import APIRouter, Depends, Header, Query, Request, Response
from sqlalchemy import asc, desc
from sqlalchemy.orm import Session

from database import get_db
from models import AttachmentEntry, ChecklistRoot
from utils.odata import SERVICE_ROOT, format_datetime, odata_payload
from utils.odata_response import odata_entity
from utils.time import now_utc
from api.gateway_operations import _apply_save_attachments, _hex, _media_upload_payload
from api.gateway_core import (
    _err, _reject_expand, _entity_key, _boundary_root_key, _boundary_parent_key,
    _entity_metadata, _load_root_or_error, _require_permission,
)
from api.gateway_serializers import _decode_slug, _attachment_matches_filter, _to_attachment

router = APIRouter(tags=["GatewayCanonical"])


@router.get(f"{SERVICE_ROOT}/AttachmentFolderSet")
def attachment_folder_set(filter: str | None = Query(None, alias="$filter"), db: Session = Depends(get_db)):
    folder_keys = set()
    rows = []
    for entry in db.query(AttachmentEntry).order_by(asc(AttachmentEntry.folder_key), asc(AttachmentEntry.changed_on)).all():
        if not str(entry.folder_key or "").strip() or entry.folder_key in folder_keys:
            continue
        if not _attachment_matches_filter(entry, filter):
            continue
        folder_keys.add(entry.folder_key)
        folder_key_str = str(entry.folder_key)
        rows.append({
            "__metadata": _entity_metadata("AttachmentFolder", "AttachmentFolderSet", folder_key_str),
            "FolderKey": folder_key_str,
            "DB_KEY": _hex(entry.root_id),
            "Title": folder_key_str,
            "CreatedOn": format_datetime(entry.created_on),
            "ChangedOn": format_datetime(entry.changed_on),
        })
    folders = rows
    return odata_payload(folders)


@router.get(f"{SERVICE_ROOT}/AttachmentSet")
def attachment_set(filter: str | None = Query(None, alias="$filter"), expand: str | None = Query(None, alias="$expand"), db: Session = Depends(get_db)):
    if (err := _reject_expand(expand)):
        return err
    rows = [
        _to_attachment(entry, db=db)
        for entry in db.query(AttachmentEntry).order_by(desc(AttachmentEntry.changed_on), desc(AttachmentEntry.created_on)).all()
        if _attachment_matches_filter(entry, filter)
    ]
    return odata_payload(rows)


@router.get(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})")
def attachment_get(entity_key: str, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    return odata_entity(_to_attachment(entry, db=db))


@router.get(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})/$value")
def attachment_value(entity_key: str, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    payload = Path(entry.storage_path or "").read_bytes() if Path(entry.storage_path or "").exists() else b""
    return Response(content=payload, media_type=str(entry.mime_type or "application/octet-stream"))


@router.post(f"{SERVICE_ROOT}/AttachmentSet")
async def attachment_create(
    request: Request,
    db_key: str | None = Header(None, alias="X-DB-Key"),
    parent_key: str | None = Header(None, alias="X-Parent-Key"),
    folder_key: str | None = Header(None, alias="X-Folder-Key"),
    category_key: str | None = Header(None, alias="X-Category-Key"),
    attachment_key: str | None = Header(None, alias="X-Attachment-Key"),
    description: str | None = Header(None, alias="X-Description"),
    file_name: str | None = Header(None, alias="X-File-Name"),
    slug: str | None = Header(None, alias="Slug"),
    content_type: str | None = Header(None),
    db: Session = Depends(get_db)
):
    body = {}
    raw_body = await request.body()

    if not raw_body:
        try:
            body = await request.json()
        except (json.JSONDecodeError, ValueError):
            body = {}

    if raw_body:
        body = _media_upload_payload(
            db_key=_boundary_root_key({"DB_KEY": db_key}, db_key),
            parent_key=_boundary_parent_key({"PARENT_KEY": parent_key}, parent_key) or _boundary_root_key({"DB_KEY": db_key}, db_key),
            folder_key=str(folder_key or "").strip(),
            category_key=str(category_key or "").strip() or "GEN",
            file_name=_decode_slug(slug) or str(file_name or "").strip() or "attachment",
            mime_type=str(content_type or "application/octet-stream").split(";", 1)[0].strip() or "application/octet-stream",
            description=str(description or "").strip(),
            body=raw_body,
            attachment_key=str(attachment_key or "").strip(),
        )

    root, err = _load_root_or_error(db, _boundary_root_key(body, body.get("PARENT_KEY")))
    if err:
        return err
    if (err := _require_permission(db, request, root, "edit")):
        return err
    try:
        _apply_save_attachments(db, root, [body], allow_media_content=True)
    except ValueError as ex:
        db.rollback()
        if str(ex) == "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN":
            return _err(400, "ATTACHMENT_BASE64_SAVE_PATH_FORBIDDEN", "Attachment upload must use media stream endpoint")
        return _err(400, "INVALID_ATTACHMENT_PAYLOAD", "Attachment payload is invalid")
    except RuntimeError as ex:
        db.rollback()
        return ex.args[0]
    root.changed_on = now_utc()
    root.version_number = int(root.version_number or 0) + 1
    db.commit()
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.root_id == root.id).order_by(desc(AttachmentEntry.created_on), desc(AttachmentEntry.changed_on)).first()
    return odata_entity(_to_attachment(entry, db=db))


@router.patch(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})")
def attachment_update(entity_key: str, payload: dict, request: Request, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    parent_root = db.query(ChecklistRoot).filter(ChecklistRoot.id == entry.root_id).first()
    if parent_root and (err := _require_permission(db, request, parent_root, "edit")):
        return err
    if "Description" in payload:
        entry.description = str(payload.get("Description") or "").strip()
    if "CategoryKey" in payload:
        entry.category_key = str(payload.get("CategoryKey") or entry.category_key or "GEN").strip() or "GEN"
    if payload.get("FileName"):
        entry.file_name = str(payload.get("FileName") or entry.file_name or "").strip()
    entry.changed_on = now_utc()
    if parent_root:
        parent_root.changed_on = now_utc()
        parent_root.version_number = int(parent_root.version_number or 0) + 1
    db.commit()
    return odata_entity(_to_attachment(entry, db=db))


@router.delete(f"{SERVICE_ROOT}/AttachmentSet({{entity_key}})")
def attachment_delete(entity_key: str, request: Request, db: Session = Depends(get_db)):
    key = _entity_key(entity_key)
    entry = db.query(AttachmentEntry).filter(AttachmentEntry.id == key).first()
    if not entry:
        return _err(404, "NOT_FOUND", "Attachment not found")
    parent_root = db.query(ChecklistRoot).filter(ChecklistRoot.id == entry.root_id).first()
    if parent_root and (err := _require_permission(db, request, parent_root, "edit")):
        return err
    path = Path(entry.storage_path or "")
    if path.exists():
        path.unlink()
    db.delete(entry)
    if parent_root:
        parent_root.changed_on = now_utc()
    db.commit()
    return Response(status_code=204)
