from collections.abc import Mapping

from sqlalchemy import and_, not_, or_
from sqlalchemy.orm.attributes import InstrumentedAttribute

from utils.filter_ast import BoolOp, Comparison, FuncCall, Not, parse_filter_ast
from utils.filter_errors import FilterSyntaxError
from utils.key_normalizer import hex_to_storage_key

_OPERATORS = {"eq": "__eq__", "ne": "__ne__", "gt": "__gt__", "lt": "__lt__", "ge": "__ge__", "le": "__le__"}


def _is_key_like_column(col: InstrumentedAttribute) -> bool:
    """True for any primary-key or foreign-key column, detected via SQLAlchemy's own
    schema introspection rather than a hardcoded column-name allowlist (previously
    {"id", "root_id"}) - so a future model's differently-named PK/FK doesn't silently
    fall outside RAW16 hex32 -> stored-UUID normalization just because nobody remembered
    to add its name here. Safe to apply broadly: hex_to_storage_key() is a no-op passthrough
    for any value that isn't itself hex32-shaped, so this can't misfire on a genuine
    non-UUID primary key (e.g. AppUserProfile.uname)."""
    try:
        column = col.property.columns[0]
    except (AttributeError, IndexError):
        return False
    return bool(column.primary_key or column.foreign_keys)


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
            if isinstance(value, str) and _is_key_like_column(col):
                value = hex_to_storage_key(value)
            return getattr(col, _OPERATORS[node.op])(value)
        raise FilterSyntaxError(f"Unsupported $filter AST node: {node!r}")
