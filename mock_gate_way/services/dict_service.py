from datetime import date as date_type

from sqlalchemy.orm import Session

from models import DictionaryItem


class DictService:
    @staticmethod
    def get_items(db: Session, domain: str, as_of: str | None = None):
        query = db.query(DictionaryItem).filter(DictionaryItem.domain == domain)
        if as_of:
            parsed_date = date_type.fromisoformat(as_of)
            query = query.filter(
                DictionaryItem.begda <= parsed_date,
                (DictionaryItem.endda.is_(None)) | (DictionaryItem.endda >= parsed_date),
            )

        items = query.order_by(DictionaryItem.key).all()
        return [{"key": item.key, "text": item.text} for item in items]
