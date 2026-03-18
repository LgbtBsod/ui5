from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from api._hierarchy import hierarchy_value_response
from database import get_db

router = APIRouter(prefix="/location", tags=["Location"])


@router.get("")
def get_locations(date: str, db: Session = Depends(get_db)):
    return hierarchy_value_response(db, date)
