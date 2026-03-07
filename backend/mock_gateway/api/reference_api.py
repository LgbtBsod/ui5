from datetime import date as date_type

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from database import get_db
from services.dict_service import DictService
from services.hierarchy_service import HierarchyService
from services.person_service import PersonService

router = APIRouter(prefix="/reference", tags=["Reference"])


@router.get("/bundle")
def get_reference_bundle(date: str | None = None, db: Session = Depends(get_db)):
    s_date = date or date_type.today().isoformat()
    d_parsed = date_type.fromisoformat(s_date)

    return {
        "value": {
            "persons": PersonService.suggest(db, search="", checklist_id=None),
            "dictionaries": {
                "LPC": DictService.get_items(db, domain="LPC", as_of=s_date),
                "PROFESSION": DictService.get_items(db, domain="PROFESSION", as_of=s_date),
            },
            "locations": HierarchyService.get_tree(db, d_parsed),
            "variables": {
                "asOfDate": s_date,
                "source": "gateway_reference_bundle"
            }
        }
    }
