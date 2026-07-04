from collections.abc import Mapping

from sqlalchemy import and_, not_, or_
from sqlalchemy.orm.attributes import InstrumentedAttribute

from utils.filter_ast import BoolOp, Comparison, FuncCall, Not, parse_filter_ast
from utils.filter_errors import FilterSyntaxError
from utils.key_normalizer import hex_to_storage_key

_OPERATORS = {"eq": "__eq__", "ne": "__ne__", "gt": "__gt__", "lt": "__lt__", "ge": "__ge__", "le": "__le__"}

# SQLAlchemy attribute names that store RAW16-derived entity keys as canonical dashed
# UUID strings (see models.py). $filter literals compared against these columns arrive
# from the frontend as hex32 (per the BINTOHEX wire convention) and must be normalized
# the same way BoundaryResolver.resolve_key normalizes path-segment keys - otherwise
# `$filter=PARENT_KEY eq 'HEX32...'` would silently never match any row.
_KEY_COLUMNS = {"id", "root_id"}


class ODataFilterParser:
    """SQL compiler for the shared $filter AST (see utils.filter_ast)."""

    @staticmethod
    def _resolve_column(model, field_name: str, field_map: Mapping[str, str] | None):
        mapped = field_map.get(field_name, field_name) if field_map else field_name
        return getattr(model, mapped, None)

    @staticmethod
    def parse(model, filter_string: str, field_map: Mapping[str, str] | None = None):
        if not filter_string:
            return None
        ast = parse_filter_ast(filter_string)
        if ast is None:
            return None
        try:
            return ODataFilterParser._compile(ast, model, field_map)
        except FilterSyntaxError:
            raise
        except Exception as exc:
            raise FilterSyntaxError(f"Could not compile $filter expression: {filter_string!r}") from exc

    @staticmethod
    def _compile(node, model, field_map):
        if isinstance(node, BoolOp):
            left = ODataFilterParser._compile(node.left, model, field_map)
            right = ODataFilterParser._compile(node.right, model, field_map)
            return and_(left, right) if node.op == "and" else or_(left, right)
        if isinstance(node, Not):
            return not_(ODataFilterParser._compile(node.operand, model, field_map))
        if isinstance(node, FuncCall):
            col: InstrumentedAttribute = ODataFilterParser._resolve_column(model, node.field, field_map)
            if col is None:
                raise FilterSyntaxError(f"Unknown field {node.field!r}")
            escaped = str(node.value).replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_")
            if node.name == "startswith":
                return col.ilike(f"{escaped}%", escape="\\")
            return col.ilike(f"%{escaped}%", escape="\\")
        if isinstance(node, Comparison):
            col: InstrumentedAttribute = ODataFilterParser._resolve_column(model, node.field, field_map)
            if col is None:
                raise FilterSyntaxError(f"Unknown field {node.field!r}")
            value = node.value
            if isinstance(value, str) and getattr(col, "key", None) in _KEY_COLUMNS:
                value = hex_to_storage_key(value)
            return getattr(col, _OPERATORS[node.op])(value)
        raise FilterSyntaxError(f"Unsupported $filter AST node: {node!r}")
