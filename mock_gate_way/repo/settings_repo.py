from __future__ import annotations

from sqlalchemy.orm import Session

from models import FrontendRuntimeSettings


class SettingsRepo:
    def __init__(self, db: Session):
        self.db = db

    def get_global(self):
        row = self.db.query(FrontendRuntimeSettings).order_by(FrontendRuntimeSettings.changed_on.desc()).first()
        if not row:
            row = FrontendRuntimeSettings(environment="default")
            self.db.add(row)
            self.db.commit()
            self.db.refresh(row)
        return row
