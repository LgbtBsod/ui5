from sqlalchemy import func
from sqlalchemy.orm import Session

from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot
from services.lock_service import LockService
from utils.etag import format_etag
from utils.time import now_utc


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

        root.changed_by = user_id
        root.changed_on = now_utc()
        db.commit()
        db.refresh(root)
        return root

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
