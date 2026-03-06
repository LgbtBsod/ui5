import logging
import re
from collections.abc import Mapping

from sqlalchemy import and_, not_, or_
from sqlalchemy.orm.attributes import InstrumentedAttribute

logger = logging.getLogger("gateway.filter")


class ODataFilterParser:
    OPERATORS = {"eq": "__eq__", "ne": "__ne__", "gt": "__gt__", "lt": "__lt__", "ge": "__ge__", "le": "__le__"}

    @staticmethod
    def _tokenize(filter_string: str) -> list[str]:
        pattern = r"substringof\(|contains\(|\(|\)|,|'[^']*'|\b(?:and|or|not|eq|ne|gt|ge|lt|le|true|false)\b|[A-Za-z_][A-Za-z0-9_]*|-?\d+(?:\.\d+)?"
        return [t for t in re.findall(pattern, filter_string, flags=re.IGNORECASE) if t and not t.isspace()]

    @staticmethod
    def _resolve_column(model, field_name: str, field_map: Mapping[str, str] | None):
        mapped = field_map.get(field_name, field_name) if field_map else field_name
        return getattr(model, mapped, None)

    @staticmethod
    def parse(model, filter_string: str, field_map: Mapping[str, str] | None = None):
        if not filter_string:
            return None
        tokens = ODataFilterParser._tokenize(filter_string)
        idx = 0

        def parse_expr():
            nonlocal idx
            node = parse_term()
            while idx < len(tokens) and tokens[idx].lower() == "or":
                idx += 1
                node = or_(node, parse_term())
            return node

        def parse_term():
            nonlocal idx
            node = parse_factor()
            while idx < len(tokens) and tokens[idx].lower() == "and":
                idx += 1
                node = and_(node, parse_factor())
            return node

        def parse_factor():
            nonlocal idx
            if idx < len(tokens) and tokens[idx].lower() == "not":
                idx += 1
                return not_(parse_factor())
            if idx < len(tokens) and tokens[idx] == "(":
                idx += 1
                node = parse_expr()
                if idx < len(tokens) and tokens[idx] == ")":
                    idx += 1
                return node
            return parse_predicate()

        def lit(token: str):
            if token.startswith("'") and token.endswith("'"):
                return token[1:-1]
            if token.lower() in {"true", "false"}:
                return token.lower() == "true"
            if token.isdigit() or (token.startswith("-") and token[1:].isdigit()):
                return int(token)
            return token

        def parse_predicate():
            nonlocal idx
            token = tokens[idx]
            lower = token.lower()
            if lower in {"contains(", "substringof("}:
                idx += 1
                first = lit(tokens[idx]); idx += 1
                if tokens[idx] == ",": idx += 1
                second = lit(tokens[idx]); idx += 1
                if tokens[idx] == ")": idx += 1
                if lower == "contains(":
                    field, value = first, second
                else:
                    value, field = first, second
                col: InstrumentedAttribute = ODataFilterParser._resolve_column(model, str(field), field_map)
                if col is None:
                    return True
                expr = col.ilike(f"%{value}%")
                if idx + 1 < len(tokens) and tokens[idx].lower() == "eq":
                    idx += 1
                    bool_val = lit(tokens[idx]); idx += 1
                    return expr if bool_val else not_(expr)
                return expr

            field = token
            idx += 1
            if idx >= len(tokens):
                return True
            op = tokens[idx].lower(); idx += 1
            if idx >= len(tokens):
                return True
            value = lit(tokens[idx]); idx += 1
            col: InstrumentedAttribute = ODataFilterParser._resolve_column(model, field, field_map)
            if col is None or op not in ODataFilterParser.OPERATORS:
                return True
            return getattr(col, ODataFilterParser.OPERATORS[op])(value)

        try:
            result = parse_expr()
            return result
        except Exception as exc:  # noqa: BLE001
            logger.warning("Failed to parse filter %s: %s", filter_string, exc)
            return None
