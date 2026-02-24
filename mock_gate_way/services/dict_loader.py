import json
import uuid
from datetime import date
from sqlalchemy.orm import Session
from models import DictionaryItem


def load_dictionary(db: Session, path: str, domain: str):

    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)

    items = data.get(domain.lower(), [])

    for item in items:

        exists = db.query(DictionaryItem).filter(
            DictionaryItem.domain == domain,
            DictionaryItem.key == item["key"]
        ).first()

        if not exists:
            db.add(DictionaryItem(
                id=str(uuid.uuid4()),
                domain=domain,
                key=item["key"],
                text=item["text"],
                begda=date(2000, 1, 1),
                endda=None
            ))

    db.commit()