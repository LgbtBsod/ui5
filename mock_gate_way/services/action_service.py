from sqlalchemy.orm import Session

from models import ChecklistRoot
from utils.time import now_utc


class ActionService:
    @staticmethod
    def set_status(db: Session, root_id: str, status: str, user_id: str):
        root = db.query(ChecklistRoot).filter(ChecklistRoot.id == root_id, ChecklistRoot.is_deleted.is_(False)).first()
        if not root:
            raise ValueError("NOT_FOUND")
        root.status = status
        root.changed_by = user_id
        root.changed_on = now_utc()
        db.commit()
        return {"status": "OK", "checklist_status": status}
