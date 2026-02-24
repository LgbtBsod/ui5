from datetime import date as date_type

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from services.hierarchy_service import HierarchyService

router = APIRouter(prefix="/hierarchy", tags=["Hierarchy"])


@router.get("")
def get_hierarchy(date: str, db: Session = Depends(get_db)):
    parsed_date = date_type.fromisoformat(date)
    return {"value": HierarchyService.get_tree(db, parsed_date)}
