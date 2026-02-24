# services/checklist_service.py

from sqlalchemy.orm import Session
from models import ChecklistRoot, ChecklistCheck, ChecklistBarrier
from services.lock_service import LockService
from datetime import datetime

from sqlalchemy import func
from models import ChecklistRoot, Barrier, Check


def now():
    return datetime.utcnow()


# 🔥 Универсальная проверка LPC
def lpc_allows_barriers(lpc: str) -> bool:
    if not lpc or len(lpc) < 2:
        return False
    try:
        level = int(lpc[1:])
        return level >= 2
    except:
        return False


@staticmethod
def calculate_etag(db: Session, root_id: str):
    root_date = db.query(ChecklistRoot.changed_on)\
        .filter(ChecklistRoot.id == root_id)\
        .scalar()

    barrier_date = db.query(func.max(Barrier.changed_on))\
        .filter(Barrier.root_id == root_id)\
        .scalar()

    check_date = db.query(func.max(Check.changed_on))\
        .filter(Check.root_id == root_id)\
        .scalar()

    dates = [d for d in [root_date, barrier_date, check_date] if d]

    if not dates:
        return None

    return max(dates)
    
class ChecklistService:

    # CREATE
    @staticmethod
    def create(db: Session, checklist_id: str, lpc: str, user_id: str):
        root = ChecklistRoot(
            checklist_id=checklist_id,
            lpc=lpc,
            created_by=user_id,
            changed_by=user_id
        )

        db.add(root)
        db.commit()
        db.refresh(root)

        return root

    # READ
    @staticmethod
    def get(db: Session, root_id: str, expand: bool = False):
        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted == False
        ).first()

        if not root:
            return None

        result = {
            "id": root.id,
            "checklist_id": root.checklist_id,
            "lpc": root.lpc,
            "status": root.status,
            "changed_on": root.changed_on
        }

        if expand:
            checks = db.query(ChecklistCheck).filter(
                ChecklistCheck.root_id == root_id
            ).all()

            barriers = db.query(ChecklistBarrier).filter(
                ChecklistBarrier.root_id == root_id
            ).all()

            result["checks"] = [
                {
                    "id": c.id,
                    "text": c.text,
                    "status": c.status,
                    "position": c.position
                } for c in checks
            ]

            result["barriers"] = [
                {
                    "id": b.id,
                    "description": b.description,
                    "is_active": b.is_active,
                    "position": b.position
                } for b in barriers
            ]

        return result

    # AUTOSAVE (partial update)
    @staticmethod
    def autosave(db: Session, root_id: str, user_id: str, data: dict, force: bool = False):
        LockService.validate_lock(db, root_id, user_id)

        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id
        ).first()

        if not root:
            raise Exception("NOT_FOUND")

        # 🔥 LPC downgrade logic
        if "lpc" in data:
            new_lpc = data["lpc"]

            if not lpc_allows_barriers(new_lpc):
                existing_barriers = db.query(ChecklistBarrier).filter(
                    ChecklistBarrier.root_id == root_id
                ).all()

                if existing_barriers:
                    if not force:
                        raise Exception("CONFIRM_DOWNGRADE_REQUIRED")

                    # удаляем barriers если force=True
                    for b in existing_barriers:
                        db.delete(b)

        # разрешённые поля
        allowed_fields = ["lpc", "status"]

        for field in allowed_fields:
            if field in data:
                setattr(root, field, data[field])

        root.changed_by = user_id
        root.changed_on = now()

        db.commit()
        db.refresh(root)

        return root

    # ADD BARRIER
    @staticmethod
    def add_barrier(db: Session, root_id: str, user_id: str, description: str, position: int):
        LockService.validate_lock(db, root_id, user_id)

        root = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted == False
        ).first()

        if not root:
            raise Exception("NOT_FOUND")

        if not lpc_allows_barriers(root.lpc):
            raise Exception("BARRIERS_NOT_ALLOWED_FOR_LPC")

        barrier = ChecklistBarrier(
            root_id=root_id,
            description=description,
            position=position
        )

        db.add(barrier)

        root.changed_on = now()
        root.changed_by = user_id

        db.commit()
        db.refresh(barrier)

        return barrier

    # COPY
    @staticmethod
    def copy(db: Session, root_id: str, user_id: str):
        source = db.query(ChecklistRoot).filter(
            ChecklistRoot.id == root_id,
            ChecklistRoot.is_deleted == False
        ).first()

        if not source:
            raise Exception("NOT_FOUND")

        new_root = ChecklistRoot(
            checklist_id=f"{source.checklist_id}_COPY",
            lpc=source.lpc,
            status="01",
            created_by=user_id,
            changed_by=user_id
        )

        db.add(new_root)
        db.commit()
        db.refresh(new_root)

        # copy checks
        checks = db.query(ChecklistCheck).filter(
            ChecklistCheck.root_id == root_id
        ).all()

        for c in checks:
            new_check = ChecklistCheck(
                root_id=new_root.id,
                text=c.text,
                status="PENDING",
                position=c.position
            )
            db.add(new_check)

        # copy barriers only if allowed
        if lpc_allows_barriers(source.lpc):
            barriers = db.query(ChecklistBarrier).filter(
                ChecklistBarrier.root_id == root_id
            ).all()

            for b in barriers:
                new_barrier = ChecklistBarrier(
                    root_id=new_root.id,
                    description=b.description,
                    is_active=b.is_active,
                    position=b.position
                )
                db.add(new_barrier)

        db.commit()

        return new_root