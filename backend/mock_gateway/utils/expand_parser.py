
from sqlalchemy.orm import joinedload


ALLOWED_EXPANDS = {
    "ChecklistRoot": {"checks", "barriers"},
    "ChecklistCheck": set(),
    "ChecklistBarrier": set(),
    "AttachmentEntry": set(),
}


class ExpandParser:

    @staticmethod
    def apply(query, model, expand_string: str):

        if not expand_string:
            return query

        model_name = model.__name__
        allowed = ALLOWED_EXPANDS.get(model_name, set())
        relations = [e.strip() for e in expand_string.split(",")]

        for rel in relations:
            if rel in allowed and hasattr(model, rel):
                query = query.options(joinedload(rel))

        return query
