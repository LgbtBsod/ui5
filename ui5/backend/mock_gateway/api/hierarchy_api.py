from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from api._hierarchy import hierarchy_value_response
from database import get_db

router = APIRouter(prefix="/hierarchy", tags=["Hierarchy"])


@router.get("")
def get_hierarchy(date: str, db: Session = Depends(get_db)):
    return hierarchy_value_response(db, date)
