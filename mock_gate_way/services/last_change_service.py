# services/last_change_service.py

from sqlalchemy.orm import Session
from models import LastChangeSet
from datetime import datetime


def now():
    return datetime.utcnow()


class LastChangeService:

    @staticmethod
    def touch_entity(db: Session, entity_name: str, entity_id: str = None):
        entry = db.query(LastChangeSet).filter(
            LastChangeSet.entity_name == entity_name,
            LastChangeSet.entity_id == entity_id
        ).first()

        if not entry:
            entry = LastChangeSet(
                entity_name=entity_name,
                entity_id=entity_id,
                last_change_timestamp=now()
            )
            db.add(entry)
        else:
            entry.last_change_timestamp = now()

        db.commit()

    @staticmethod
    def get_last_change(db: Session, entity_name: str):
        entry = db.query(LastChangeSet).filter(
            LastChangeSet.entity_name == entity_name,
            LastChangeSet.entity_id == None
        ).first()

        if not entry:
            return None

        return entry.last_change_timestamp