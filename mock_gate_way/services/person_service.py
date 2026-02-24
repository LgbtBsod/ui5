from sqlalchemy import func
from sqlalchemy.orm import Session

from config import MAX_SUGGEST_RESULTS
from models import ChecklistRoot, Person


class PersonService:
    @staticmethod
    def suggest(db: Session, search: str | None = None, checklist_id: str | None = None):
        query = db.query(Person)

        if checklist_id:
            root = db.query(ChecklistRoot).filter(ChecklistRoot.id == checklist_id).first()
            if root and hasattr(root, "date_check") and root.date_check:
                query = query.filter(
                    Person.begda <= root.date_check,
                    (Person.endda.is_(None)) | (Person.endda >= root.date_check),
                )

        if search:
            full_name_expr = func.trim(Person.last_name + " " + Person.first_name + " " + Person.middle_name)
            query = query.filter(func.lower(full_name_expr).like(f"%{search.lower()}%"))

        results = query.limit(MAX_SUGGEST_RESULTS).all()
        return [
            {
                "perner": p.perner,
                "fullName": f"{p.last_name} {p.first_name} {p.middle_name}".strip(),
                "position": p.position,
                "orgUnit": p.org_unit,
            }
            for p in results
        ]
