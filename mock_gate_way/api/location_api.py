from datetime import date as date_type

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from services.location_service import LocationService

router = APIRouter(prefix="/location", tags=["Location"])


@router.get("")
def get_locations(date: str, db: Session = Depends(get_db)):
    parsed_date = date_type.fromisoformat(date)
    return {"value": LocationService.get_tree(db, parsed_date)}
