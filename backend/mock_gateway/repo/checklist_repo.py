from __future__ import annotations

from sqlalchemy.orm import Session

from models import ChecklistBarrier, ChecklistCheck, ChecklistRoot


class ChecklistRepo:
    def __init__(self, db: Session):
        self.db = db

    def get_root(self, root_id: str):
        return self.db.query(ChecklistRoot).filter(ChecklistRoot.id == root_id, ChecklistRoot.is_deleted.isnot(True)).first()

    def get_basic(self, root_id: str):
        return self.get_root(root_id)

    def list_checks(self, root_id: str, top: int, skip: int):
        return self.db.query(ChecklistCheck).filter(ChecklistCheck.root_id == root_id).offset(skip).limit(top).all()

    def list_barriers(self, root_id: str, top: int, skip: int):
        return self.db.query(ChecklistBarrier).filter(ChecklistBarrier.root_id == root_id).offset(skip).limit(top).all()

    def search(self):
        return self.db.query(ChecklistRoot).filter(ChecklistRoot.is_deleted.isnot(True)).all()

    def apply_autosave(self, _payload: dict):
        return None

    def apply_savechanges(self, _payload: dict):
        return None

    def set_status(self, root_id: str, status: str):
        root = self.get_root(root_id)
        if root:
            root.status = status
            self.db.commit()
        return root

    def get_last_change(self, root_id: str):
        root = self.get_root(root_id)
        return root.changed_on if root else None
