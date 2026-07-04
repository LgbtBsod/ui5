import re
from collections.abc import Mapping

from sqlalchemy import and_, not_, or_
from sqlalchemy.orm.attributes import InstrumentedAttribute

from utils.filter_errors import FilterSyntaxError
from utils.key_normalizer import hex_to_storage_key

# SQLAlchemy attribute names that store RAW16-derived entity keys as canonical dashed
# UUID strings (see models.py). $filter literals compared against these columns arrive
# from the frontend as hex32 (per the BINTOHEX wire convention) and must be normalized
# the same way BoundaryResolver.resolve_key normalizes path-segment keys - otherwise
# `$filter=PARENT_KEY eq 'HEX32...'` would silently never match any row.
_KEY_COLUMNS = {"id", "root_id"}


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
                    raise FilterSyntaxError(f"Unknown field {field!r}")
                escaped = str(value).replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_")
                expr = col.ilike(f"%{escaped}%", escape="\\")
                if idx + 1 < len(tokens) and tokens[idx].lower() == "eq":
                    idx += 1
                    bool_val = lit(tokens[idx]); idx += 1
                    return expr if bool_val else not_(expr)
                return expr

            field = token
            idx += 1
            if idx >= len(tokens):
                raise FilterSyntaxError(f"Incomplete comparison for field {field!r}")
            op = tokens[idx].lower(); idx += 1
            if idx >= len(tokens):
                raise FilterSyntaxError(f"Missing comparison value for field {field!r}")
            value = lit(tokens[idx]); idx += 1
            col: InstrumentedAttribute = ODataFilterParser._resolve_column(model, field, field_map)
            if col is None:
                raise FilterSyntaxError(f"Unknown field {field!r}")
            if op not in ODataFilterParser.OPERATORS:
                raise FilterSyntaxError(f"Unsupported operator {op!r}")
            if isinstance(value, str) and getattr(col, "key", None) in _KEY_COLUMNS:
                value = hex_to_storage_key(value)
            return getattr(col, ODataFilterParser.OPERATORS[op])(value)

        try:
            return parse_expr()
        except FilterSyntaxError:
            raise
        except Exception as exc:
            raise FilterSyntaxError(f"Could not parse $filter expression: {filter_string!r}") from exc
