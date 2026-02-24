from datetime import date as date_type

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from models import DictionaryItem

router = APIRouter(prefix="/dictionary", tags=["Dictionary"])


@router.get("")
def get_dictionary(domain: str, date: str = None, db: Session = Depends(get_db)):
    query = db.query(DictionaryItem).filter(DictionaryItem.domain == domain)

    if date:
        parsed_date = date_type.fromisoformat(date)
        query = query.filter(
            DictionaryItem.begda <= parsed_date,
            (DictionaryItem.endda.is_(None)) | (DictionaryItem.endda >= parsed_date),
        )

    items = query.all()
    return {"value": [{"key": item.key, "text": item.text} for item in items]}
