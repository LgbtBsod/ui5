from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from services.person_service import PersonService

router = APIRouter(prefix="/persons", tags=["Persons"])


@router.get("/suggest")
def suggest_person(search: str = None, checklist_id: str = None, db: Session = Depends(get_db)):
    return {"value": PersonService.suggest(db, search=search, checklist_id=checklist_id)}
