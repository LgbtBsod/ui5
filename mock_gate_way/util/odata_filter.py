from sqlalchemy import and_, or_
from sqlalchemy.orm.attributes import InstrumentedAttribute


class ODataFilterParser:

    OPERATORS = {
        "eq": "__eq__",
        "ne": "__ne__",
        "gt": "__gt__",
        "lt": "__lt__",
        "ge": "__ge__",
        "le": "__le__"
    }

    @staticmethod
    def parse(model, filter_string: str):
        if not filter_string:
            return None

        tokens = filter_string.split()

        expressions = []
        current_logic = None

        i = 0
        while i < len(tokens):
            token = tokens[i]

            if token.lower() in ("and", "or"):
                current_logic = token.lower()
                i += 1
                continue

            field = token
            operator = tokens[i + 1]
            value = tokens[i + 2]

            value = value.strip("'")

            column: InstrumentedAttribute = getattr(model, field)

            if operator in ODataFilterParser.OPERATORS:
                expr = getattr(column, ODataFilterParser.OPERATORS[operator])(value)
            elif operator.startswith("contains"):
                expr = column.ilike(f"%{value}%")
            else:
                raise Exception("Unsupported operator")

            expressions.append(expr)
            i += 3

        if not expressions:
            return None

        result = expressions[0]

        for idx in range(1, len(expressions)):
            if current_logic == "or":
                result = or_(result, expressions[idx])
            else:
                result = and_(result, expressions[idx])

        return result