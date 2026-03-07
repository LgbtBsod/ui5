from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from services.dict_service import DictService

router = APIRouter(prefix="/dict", tags=["Dictionary"])
legacy_router = APIRouter(prefix="/dictionary", tags=["Dictionary"])


def _get_dictionary(domain: str, date: str | None, db: Session):
    return {"value": DictService.get_items(db, domain=domain, as_of=date)}


@router.get("")
def get_dictionary(domain: str, date: str = None, db: Session = Depends(get_db)):
    return _get_dictionary(domain, date, db)


@legacy_router.get("")
def get_dictionary_legacy(domain: str, date: str = None, db: Session = Depends(get_db)):
    return _get_dictionary(domain, date, db)
