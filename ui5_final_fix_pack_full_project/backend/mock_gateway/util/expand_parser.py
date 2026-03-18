
from sqlalchemy.orm import joinedload


class ExpandParser:

    @staticmethod
    def apply(query, model, expand_string: str):

        if not expand_string:
            return query

        relations = [e.strip() for e in expand_string.split(",")]

        for rel in relations:
            if hasattr(model, rel):
                query = query.options(joinedload(rel))

        return query