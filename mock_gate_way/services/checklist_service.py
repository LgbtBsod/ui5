from sqlalchemy import func
import json
from sqlalchemy.orm import Session

from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot, SaveRequestLedger
from services.lock_service import LockService
from utils.etag import format_etag
from utils.time import now_utc


BASIC_FIELD_MAP = {
    "date": "date",
    "equipment": "equipment",
    "LPC_TEXT": "lpc_text",
    "OBSERVER_FULLNAME": "observer_fullname",
    "OBSERVER_PERNER": "observer_perner",
    "OBSERVER_POSITION": "observer_position",
    "OBSERVER_ORGUNIT": "observer_orgunit",
    "OBSERVER_INTEGRATION_NAME": "observer_integration_name",
    "OBSERVED_FULLNAME": "observed_fullname",
    "OBSERVED_PERNER": "observed_perner",
    "OBSERVED_POSITION": "observed_position",
    "OBSERVED_ORGUNIT": "observed_orgunit",
    "OBSERVED_INTEGRATION_NAME": "observed_integration_name",
    "LOCATION_KEY": "location_key",
    "LOCATION_NAME": "location_name",
    "LOCATION_TEXT": "location_text",
}


def lpc_allows_barriers(lpc: str) -> bool:
    if not lpc or len(lpc) < 2:
        return False
    try:
        return int(lpc[1:]) >= 2
    except ValueError:
        return False


class ChecklistService:
    @staticmethod
    def calculate_etag(db: Session, root_id: str):
        root_date = db.query(ChecklistRoot.changed_on).filter(ChecklistRoot.id == root_id).scalar()
        barrier_date = db.query(func.max(ChecklistBarrier.changed_on)).filter(ChecklistBarrier.root_id == root_id).scalar()
        check_date = db.query(func.max(ChecklistCheck.changed_on)).filter(ChecklistCheck.root_id == root_id).scalar()

        dates = [d for d in (root_date, barrier_date, check_date) if d is not None]
        return max(dates) if dates else None

    @staticmethod
    def get_etag_value(db: Session, root_id: str) -> str | None:
        return format_etag(ChecklistService.calculate_etag(db, root_id))

    @staticmethod
    def create(db: Session, checklist_id: str, lpc: str, user_id: str):
        root = ChecklistRoot(
            checklist_id=checklist_id,
            lpc=lpc,
            created_by=user_id,
            changed_by=user_id,
        )
        db.add(root)
        db.commit()
        db.refresh(root)
        return root

    @staticmethod
    def get(db: Session, root_id: str, expand: bool = False):
        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted.is_(False),
        ).first()
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
                {"id": c.id, "text": c.text, "status": c.status, "position": c.position}
                for c in root.checks
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
    def list_checks(db: Session, root_id: str, top: int = 50, skip: int = 0):
        query = db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root_id).order_by(ChecklistCheck.position.asc())
        total = query.count()
        rows = query.offset(skip).limit(top).all()
        return {
            "value": [
                {"id": row.id, "text": row.text, "status": row.status, "position": row.position}
                for row in rows
            ],
            "count": total,
        }

    @staticmethod
    def list_barriers(db: Session, root_id: str, top: int = 50, skip: int = 0):
        query = db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root_id).order_by(ChecklistBarrier.position.asc())
        total = query.count()
        rows = query.offset(skip).limit(top).all()
        return {
            "value": [
                {
                    "id": row.id,
                    "description": row.description,
                    "is_active": row.is_active,
                    "position": row.position,
                }
                for row in rows
            ],
            "count": total,
        }


    @staticmethod
    def update_with_etag(db: Session, root_id: str, user_id: str, data: dict, if_match: str):
        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_id, ChecklistRoot.is_deleted.is_(False)).first()
        if not root:
            raise ValueError("NOT_FOUND")

        if if_match is None:
            raise ValueError("PRECONDITION_REQUIRED")

        current_etag = ChecklistService.get_etag_value(db, root_id)
        if current_etag != if_match:
            raise ValueError("ETAG_MISMATCH")

        return ChecklistService.autosave(db, root_id, user_id, data, force=data.get("force", False))

    @staticmethod
    def autosave(db: Session, root_id: str, user_id: str, data: dict, force: bool = False):
        LockService.validate_lock(db, root_id, user_id)

        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_id).first()
        if not root:
            raise ValueError("NOT_FOUND")

        if "lpc" in data and not lpc_allows_barriers(data["lpc"]):
            existing_barriers = db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root_id).all()
            if existing_barriers:
                if not force:
                    raise ValueError("CONFIRM_DOWNGRADE_REQUIRED")
                for barrier in existing_barriers:
                    db.delete(barrier)

        for field in ("lpc", "status"):
            if field in data:
                setattr(root, field, data[field])

        basic_payload = data.get("basic") if isinstance(data, dict) else None
        if isinstance(basic_payload, dict):
            for incoming_key, model_field in BASIC_FIELD_MAP.items():
                if incoming_key in basic_payload:
                    setattr(root, model_field, basic_payload.get(incoming_key) or "")

        for incoming_key, model_field in BASIC_FIELD_MAP.items():
            if incoming_key in data:
                setattr(root, model_field, data.get(incoming_key) or "")

        root.changed_by = user_id
        root.changed_on = now_utc()
        db.commit()
        db.refresh(root)
        return root


    @staticmethod
    def save_via_import(db: Session, root_id: str, user_id: str, payload: dict, is_autosave: bool = False, force: bool = False, request_guid: str | None = None):
        operation = "AUTOSAVE" if is_autosave else "SAVE"
        if request_guid:
            existing = db.query(SaveRequestLedger).filter(
                SaveRequestLedger.request_guid == request_guid,
                SaveRequestLedger.operation == operation,
                SaveRequestLedger.root_id == root_id,
                SaveRequestLedger.user_id == user_id,
            ).order_by(SaveRequestLedger.created_on.desc()).first()
            if existing:
                try:
                    return json.loads(existing.response_payload)
                except Exception:
                    pass

        LockService.validate_lock(db, root_id, user_id)

        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted.is_(False),
        ).first()
        if not root:
            raise ValueError("NOT_FOUND")

        data = payload or {}
        basic_payload = data.get("basic") if isinstance(data, dict) else {}
        checks_payload = data.get("checks") if isinstance(data, dict) else None
        barriers_payload = data.get("barriers") if isinstance(data, dict) else None

        s_lpc = data.get("lpc") or basic_payload.get("LPC_KEY") or root.lpc
        if s_lpc:
            root.lpc = s_lpc

        if isinstance(basic_payload, dict):
            for incoming_key, model_field in BASIC_FIELD_MAP.items():
                if incoming_key in basic_payload:
                    setattr(root, model_field, basic_payload.get(incoming_key) or "")

        if checks_payload is not None and (not is_autosave or force):
            db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root_id).delete()
            for i, row in enumerate(checks_payload or []):
                db.add(
                    ChecklistCheck(
                        root_id=root_id,
                        text=(row or {}).get("text") or "",
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
            db.add(SaveRequestLedger(
                request_guid=request_guid,
                operation=operation,
                root_id=root_id,
                user_id=user_id,
                response_payload=json.dumps(result, default=str),
            ))
            db.commit()

        return result

    @staticmethod
    def add_barrier(db: Session, root_id: str, user_id: str, description: str, position: int):
        LockService.validate_lock(db, root_id, user_id)

        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted.is_(False),
        ).first()
        if not root:
            raise ValueError("NOT_FOUND")
        if not lpc_allows_barriers(root.lpc):
            raise ValueError("BARRIERS_NOT_ALLOWED_FOR_LPC")

        barrier = ChecklistBarrier(root_id=root_id, description=description, position=position)
        db.add(barrier)

        root.changed_on = now_utc()
        root.changed_by = user_id
        db.commit()
        db.refresh(barrier)
        return barrier



    @staticmethod
    def replace_rows(db: Session, root_id: str, user_id: str, section: str, rows: list[dict]):
        LockService.validate_lock(db, root_id, user_id)

        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted.is_(False),
        ).first()
        if not root:
            raise ValueError("NOT_FOUND")

        if section == "checks":
            db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root_id).delete()
            for i, row in enumerate(rows or []):
                db.add(
                    ChecklistCheck(
                        root_id=root_id,
                        text=row.get("text") or "",
                        status="DONE" if row.get("result") else "PENDING",
                        position=i,
                    )
                )
        elif section == "barriers":
            if not lpc_allows_barriers(root.lpc):
                raise ValueError("BARRIERS_NOT_ALLOWED_FOR_LPC")
            db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root_id).delete()
            for i, row in enumerate(rows or []):
                db.add(
                    ChecklistBarrier(
                        root_id=root_id,
                        description=row.get("text") or row.get("description") or "",
                        is_active=bool(row.get("result", row.get("is_active", True))),
                        position=i,
                    )
                )
        else:
            raise ValueError("UNSUPPORTED_SECTION")

        root.changed_by = user_id
        root.changed_on = now_utc()
        db.commit()
        return ChecklistService.get(db, root_id, expand=True)

    @staticmethod
    def delete(db: Session, root_id: str, user_id: str):
        LockService.validate_lock(db, root_id, user_id)

        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted.is_(False),
        ).first()
        if not root:
            raise ValueError("NOT_FOUND")

        root.is_deleted = True
        root.changed_by = user_id
        root.changed_on = now_utc()
        db.commit()
        return {"status": "DELETED", "id": root_id}

    @staticmethod
    def copy(db: Session, root_id: str, user_id: str):
        source = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted.is_(False),
        ).first()
        if not source:
            raise ValueError("NOT_FOUND")

        new_root = ChecklistRoot(
            checklist_id=f"{source.checklist_id}_COPY",
            lpc=source.lpc,
            status="01",
            date=source.date,
            equipment=source.equipment,
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
            created_by=user_id,
            changed_by=user_id,
        )
        db.add(new_root)
        db.commit()
        db.refresh(new_root)

        for check in source.checks:
            db.add(
                ChecklistCheck(
                    root_id=new_root.id,
                    text=check.text,
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
                        is_active=barrier.is_active,
                        position=barrier.position,
                    )
                )

        db.commit()
        return new_root
