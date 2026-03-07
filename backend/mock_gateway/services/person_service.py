import logging
from sqlalchemy import func
from sqlalchemy.orm import Session

from config import MAX_SUGGEST_RESULTS
from models import ChecklistRoot, Person

logger = logging.getLogger("gateway.person")


class PersonService:
    @staticmethod
    def suggest(db: Session, search: str | None = None, checklist_id: str | None = None):
        logger.info("Person suggest search=%r checklist_id=%r", search, checklist_id)
        query = db.query(Person)

        if checklist_id:
            root = db.query(ChecklistRoot).filter(ChecklistRoot.id == checklist_id).first()
            root_date = getattr(root, "date", None)
            if root_date:
                query = query.filter(
                    Person.begda <= root_date,
                    (Person.endda.is_(None)) | (Person.endda >= root_date),
                )
                logger.info("Person suggest scoped by checklist date=%s", root_date)

        if search:
            full_name_expr = func.trim(Person.last_name + " " + Person.first_name + " " + Person.middle_name)
            query = query.filter(func.lower(full_name_expr).like(f"%{search.lower()}%"))

        results = query.limit(MAX_SUGGEST_RESULTS).all()
        logger.info("Person suggest returned rows=%s", len(results))
        return [
            {
                "perner": p.perner,
                "fullName": f"{p.last_name} {p.first_name} {p.middle_name}".strip(),
                "position": p.position,
                "orgUnit": p.org_unit,
            }
            for p in results
        ]
