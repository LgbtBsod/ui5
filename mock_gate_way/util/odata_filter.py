import logging

from sqlalchemy import and_, or_
from sqlalchemy.orm.attributes import InstrumentedAttribute

logger = logging.getLogger("gateway.filter")


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

        logger.info("Parsing OData filter: %s", filter_string)

        normalized = filter_string.replace("(", " ").replace(")", " ").replace(",", " ")
        tokens = [token for token in normalized.split() if token]

        expressions = []
        logic_ops = []

        i = 0
        while i < len(tokens):
            token = tokens[i]
            token_lower = token.lower()

            if token_lower in ("and", "or"):
                logic_ops.append(token_lower)
                i += 1
                continue

            if token_lower == "contains" and i + 2 < len(tokens):
                field = tokens[i + 1]
                value = tokens[i + 2].strip("'")
                if value == "":
                    logger.info("Skipping empty contains filter for field=%s", field)
                    i += 3
                    continue
                column: InstrumentedAttribute = getattr(model, field, None)
                if column is None:
                    logger.warning("Skipping unknown filter field in contains: %s", field)
                    i += 3
                    continue
                expressions.append(column.ilike(f"%{value}%"))
                i += 3
                continue

            if i + 2 >= len(tokens):
                break

            field = token
            operator = tokens[i + 1].lower()
            value = tokens[i + 2].strip("'")
            if value == "":
                logger.info("Skipping empty filter for field=%s operator=%s", field, operator)
                i += 3
                continue

            column: InstrumentedAttribute = getattr(model, field, None)
            if column is None:
                logger.warning("Skipping unknown filter field: %s", field)
                i += 3
                continue

            if operator in ODataFilterParser.OPERATORS:
                expr = getattr(column, ODataFilterParser.OPERATORS[operator])(value)
                expressions.append(expr)
            else:
                logger.warning("Unsupported filter operator=%s field=%s", operator, field)

            i += 3

        if not expressions:
            logger.info("No valid expressions built from filter")
            return None

        result = expressions[0]
        for idx in range(1, len(expressions)):
            logic = logic_ops[idx - 1] if idx - 1 < len(logic_ops) else "and"
            if logic == "or":
                result = or_(result, expressions[idx])
            else:
                result = and_(result, expressions[idx])

        logger.info("Built SQLAlchemy filter expression: %s", result)
        return result
