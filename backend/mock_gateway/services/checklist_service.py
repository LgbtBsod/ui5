import json
import shutil
import uuid
from pathlib import Path

from sqlalchemy.orm import Session

from api.gateway_mutations import _apply_save_root
from api.gateway_validators import _pick_first_present, _pick_text
from models import AttachmentEntry, ChecklistBarrier, ChecklistCheck, ChecklistRoot, SaveRequestLedger
from services.lock_service import LockService
from utils.time import now_utc

_BUKRS_BY_ORGUNIT = {
    "EHS": "1000",
    "QA": "1000",
    "LABORATORY": "1000",
    "WAREHOUSE": "1000",
    "MAINTENANCE": "2000",
    "REPAIR": "2000",
    "ENERGY": "2000",
    "PRODUCTION": "3000",
    "EXTRACTION": "3000",
    "COMPRESSOR": "3000",
    "PREPARATION": "3000",
}


def resolve_bukrs_from_observer(
    observer_perner: str | None, observer_orgunit: str | None, bukrs_hint: str | None = None
) -> str:
    hinted = str(bukrs_hint or "").strip()
    if hinted:
        return hinted

    org_unit = str(observer_orgunit or "").strip().upper()
    for needle, bukrs in _BUKRS_BY_ORGUNIT.items():
        if needle in org_unit:
            return bukrs

    perner = str(observer_perner or "").strip()
    if perner and perner[-1].isdigit():
        bucket = int(perner[-1]) % 3
        return ["1000", "2000", "3000"][bucket]

    return "1000"


def lpc_allows_barriers(lpc: str) -> bool:
    if not lpc or len(lpc) < 2:
        return False
    try:
        return int(lpc[1:]) >= 2
    except ValueError:
        return False


class ChecklistService:
    @staticmethod
    def _copy_attachment_rows(db: Session, source_root_id: str, target_root_id: str) -> None:
        entries = (
            db.query(AttachmentEntry)
            .filter(AttachmentEntry.root_id == source_root_id)
            .order_by(AttachmentEntry.created_on.asc())
            .all()
        )
        for entry in entries:
            source_path = Path(entry.storage_path)
            new_id = str(uuid.uuid4())
            target_path = source_path.with_name(new_id)
            shutil.copyfile(source_path, target_path)
            db.add(
                AttachmentEntry(
                    id=new_id,
                    root_id=target_root_id,
                    folder_key=str(entry.folder_key or target_root_id),
                    category_key=entry.category_key,
                    file_name=entry.file_name,
                    mime_type=entry.mime_type,
                    file_size=int(entry.file_size or 0),
                    storage_path=str(target_path),
                    created_on=now_utc(),
                    changed_on=now_utc(),
                )
            )

    @staticmethod
    def get(db: Session, root_id: str, expand: bool = False):
        root = (
            db.query(ChecklistRoot)
            .filter(
                ChecklistRoot.id == root_id,
                ChecklistRoot.is_deleted.isnot(True),
            )
            .first()
        )
        if not root:
            return None

        result = {
            "id": root.id,
            "checklist_id": root.checklist_id,
            "lpc": root.lpc,
            "status": root.status,
            "changed_on": root.changed_on,
            "date": root.date or "",
            "equipment": root.equipment or "",
            "bukrs": root.bukrs or "",
            "lpc_text": root.lpc_text or "",
            "observer_fullname": root.observer_fullname or "",
            "observer_perner": root.observer_perner or "",
            "observer_position": root.observer_position or "",
            "observer_orgunit": root.observer_orgunit or "",
            "observer_integration_name": root.observer_integration_name or "",
            "observed_fullname": root.observed_fullname or "",
            "observed_perner": root.observed_perner or "",
            "observed_position": root.observed_position or "",
            "observed_orgunit": root.observed_orgunit or "",
            "observed_integration_name": root.observed_integration_name or "",
            "location_key": root.location_key or "",
            "location_name": root.location_name or "",
            "location_text": root.location_text or "",
        }

        if expand:
            result["checks"] = [
                {"id": c.id, "text": c.text, "status": c.status, "position": c.position} for c in root.checks
            ]
            result["barriers"] = [
                {
                    "id": b.id,
                    "description": b.description,
                    "is_active": b.is_active,
                    "position": b.position,
                }
                for b in root.barriers
            ]
        return result

    @staticmethod
    def save_via_import(
        db: Session,
        root_id: str,
        user_id: str,
        payload: dict,
        is_autosave: bool = False,
        force: bool = False,
        request_guid: str | None = None,
        session_guid: str | None = None,
    ):
        operation = "AUTOSAVE" if is_autosave else "SAVE"
        if request_guid:
            existing = (
                db.query(SaveRequestLedger)
                .filter(
                    SaveRequestLedger.request_guid == request_guid,
                    SaveRequestLedger.operation == operation,
                    SaveRequestLedger.root_id == root_id,
                    SaveRequestLedger.user_id == user_id,
                )
                .order_by(SaveRequestLedger.created_on.desc())
                .first()
            )
            if existing:
                try:
                    return json.loads(existing.response_payload)
                except Exception as exc:
                    raise ValueError("REQUEST_LEDGER_CORRUPT") from exc

        if session_guid:
            LockService.validate_session_lock(db, root_id, session_guid)
        else:
            LockService.validate_lock(db, root_id, user_id)

        root = (
            db.query(ChecklistRoot)
            .filter(
                ChecklistRoot.id == root_id,
                ChecklistRoot.is_deleted.isnot(True),
            )
            .first()
        )
        if not root:
            raise ValueError("NOT_FOUND")

        data = payload or {}
        basic_payload = data.get("basic") if isinstance(data, dict) else {}
        checks_payload = data.get("checks") if isinstance(data, dict) else None
        barriers_payload = data.get("barriers") if isinstance(data, dict) else None

        s_lpc = data.get("lpc") or (basic_payload or {}).get("Lpc") or root.lpc
        if s_lpc:
            root.lpc = s_lpc

        if isinstance(basic_payload, dict):
            # Delegate to the same root-field mapping used by the canonical SaveChanges/
            # AutoSave path (api.gateway_mutations._apply_save_root) instead of the
            # independent BASIC_FIELD_MAP this used to carry - this fallback ("release
            # with force-save") payload speaks the same OData-ish field names as every
            # other save entry point now, not a second, divergent SAP-uppercase dialect.
            _apply_save_root(root, basic_payload, db)
            if _pick_first_present(basic_payload, "observer_integration_name", "ObserverIntegrationName") is not None:
                root.observer_integration_name = _pick_text(
                    basic_payload, "observer_integration_name", "ObserverIntegrationName"
                )
            if _pick_first_present(basic_payload, "observed_integration_name", "ObservedIntegrationName") is not None:
                root.observed_integration_name = _pick_text(
                    basic_payload, "observed_integration_name", "ObservedIntegrationName"
                )
            root.bukrs = resolve_bukrs_from_observer(
                root.observer_perner,
                root.observer_orgunit,
                root.bukrs,
            )

        if checks_payload is not None and (not is_autosave or force):
            db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root_id).delete()
            for i, row in enumerate(checks_payload or []):
                db.add(
                    ChecklistCheck(
                        root_id=root_id,
                        text=(row or {}).get("text") or "",
                        comment=(row or {}).get("comment") or "",
                        status="DONE" if (row or {}).get("result") else "PENDING",
                        position=i,
                    )
                )

        if barriers_payload is not None and (not is_autosave or force):
            if lpc_allows_barriers(root.lpc):
                db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root_id).delete()
                for i, row in enumerate(barriers_payload or []):
                    db.add(
                        ChecklistBarrier(
                            root_id=root_id,
                            description=(row or {}).get("text") or (row or {}).get("description") or "",
                            comment=(row or {}).get("comment") or "",
                            is_active=bool((row or {}).get("result", (row or {}).get("is_active", True))),
                            position=i,
                        )
                    )
            else:
                db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root_id).delete()

        root.changed_by = user_id
        root.changed_on = now_utc()
        db.commit()
        result = ChecklistService.get(db, root_id, expand=True)

        if request_guid and result:
            db.add(
                SaveRequestLedger(
                    request_guid=request_guid,
                    operation=operation,
                    root_id=root_id,
                    user_id=user_id,
                    response_payload=json.dumps(result, default=str),
                )
            )
            db.commit()

        return result

    @staticmethod
    def copy(db: Session, root_id: str, user_id: str):
        source = (
            db.query(ChecklistRoot)
            .filter(
                ChecklistRoot.id == root_id,
                ChecklistRoot.is_deleted.isnot(True),
            )
            .first()
        )
        if not source:
            raise ValueError("NOT_FOUND")
        current_time = now_utc()

        new_root = ChecklistRoot(
            checklist_id=f"{source.checklist_id}_COPY",
            lpc=source.lpc,
            status="01",
            integration_flag=bool(source.integration_flag),
            date=current_time.date().isoformat(),
            time_check=current_time.strftime("%H:%M"),
            time_zone=source.time_zone or "UTC",
            equipment=source.equipment,
            bukrs=source.bukrs,
            lpc_text=source.lpc_text,
            observer_fullname=source.observer_fullname,
            observer_perner=source.observer_perner,
            observer_position=source.observer_position,
            observer_orgunit=source.observer_orgunit,
            observer_integration_name=source.observer_integration_name,
            observed_fullname=source.observed_fullname,
            observed_perner=source.observed_perner,
            observed_position=source.observed_position,
            observed_orgunit=source.observed_orgunit,
            observed_integration_name=source.observed_integration_name,
            location_key=source.location_key,
            location_name=source.location_name,
            location_text=source.location_text,
            created_on=current_time,
            changed_on=current_time,
            created_by=user_id,
            changed_by=user_id,
            version_number=1,
        )
        db.add(new_root)
        db.commit()
        db.refresh(new_root)

        for check in source.checks:
            db.add(
                ChecklistCheck(
                    root_id=new_root.id,
                    text=check.text,
                    comment=check.comment,
                    status="PENDING",
                    position=check.position,
                )
            )

        if lpc_allows_barriers(source.lpc):
            for barrier in source.barriers:
                db.add(
                    ChecklistBarrier(
                        root_id=new_root.id,
                        description=barrier.description,
                        comment=barrier.comment,
                        is_active=barrier.is_active,
                        position=barrier.position,
                    )
                )

        ChecklistService._copy_attachment_rows(db, source.id, new_root.id)
        db.commit()
        return new_root
