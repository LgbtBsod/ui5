from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session
from sqlalchemy import and_
from database import get_db
from models import DictionaryItem
from api.dictionary_api import router as dictionary_router

app.include_router(dictionary_router)

router = APIRouter(prefix="/dictionary", tags=["Dictionary"])


@router.get("")
def get_dictionary(
    domain: str,
    date: str = None,
    db: Session = Depends(get_db)
):

    query = db.query(DictionaryItem).filter(
        DictionaryItem.domain == domain
    )

    if date:
        query = query.filter(
            DictionaryItem.begda <= date,
            (DictionaryItem.endda == None) | (DictionaryItem.endda >= date)
        )

    items = query.all()

    return {
        "value": [
            {
                "key": i.key,
                "text": i.text
            }
            for i in items
        ]
    }