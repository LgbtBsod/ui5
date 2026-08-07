from __future__ import annotations

from datetime import datetime, timezone
from typing import Callable

from utils.filter_ast import BoolOp, Comparison, FuncCall, Not, parse_filter_ast
from utils.filter_errors import FilterSyntaxError


def _date_only(v) -> str:
    raw = str(v or "")
    if raw.startswith("/Date("):
        try:
            ms = int(raw[6:-2].split("+")[0].split("-")[0])
            return datetime.fromtimestamp(ms / 1000, tz=timezone.utc).strftime("%Y-%m-%d")
        except Exception:
            return ""
    if raw.lower().startswith("datetime'") and raw.endswith("'"):
        raw = raw[9:-1]
    return raw.split("T", 1)[0][:10]


def _numeric(value):
    if isinstance(value, bool):
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def _compare(op: str, lv, rv) -> bool:
    if op == "eq":
        return lv == rv
    if op == "ne":
        return lv != rv
    if op == "gt":
        return lv > rv
    if op == "lt":
        return lv < rv
    if op == "ge":
        return lv >= rv
    if op == "le":
        return lv <= rv
    raise FilterSyntaxError(f"Unsupported operator {op!r}")


def _compile_comparison(node: Comparison, field_map: dict) -> Callable[[dict], bool]:
    fld = field_map.get(node.field, node.field) if field_map else node.field
    op, rv = node.op, node.value

    def pred(row: dict) -> bool:
        lv = row.get(fld)
        if fld == "DateCheck":
            return _compare(op, _date_only(lv), _date_only(rv))
        if isinstance(rv, bool):
            if op not in ("eq", "ne"):
                raise FilterSyntaxError(f"Operator {op!r} is not valid for a boolean field")
            return _compare(op, bool(lv), rv)
        lv_num, rv_num = _numeric(lv), _numeric(rv)
        if lv_num is not None and rv_num is not None:
            return _compare(op, lv_num, rv_num)
        return _compare(op, str(lv or ""), str(rv or ""))

    return pred


def _compile_func(node: FuncCall, field_map: dict) -> Callable[[dict], bool]:
    fld = field_map.get(node.field, node.field) if field_map else node.field
    needle = str(node.value).lower()

    def pred(row: dict) -> bool:
        haystack = str(row.get(fld, "") or "").lower()
        if node.name == "startswith":
            return haystack.startswith(needle)
        return needle in haystack  # contains() / substringof()

    return pred


def _compile(node, field_map: dict) -> Callable[[dict], bool]:
    if isinstance(node, BoolOp):
        left = _compile(node.left, field_map)
        right = _compile(node.right, field_map)
        if node.op == "and":
            return lambda row: bool(left(row) and right(row))
        return lambda row: bool(left(row) or right(row))
    if isinstance(node, Not):
        operand = _compile(node.operand, field_map)
        return lambda row: not operand(row)
    if isinstance(node, FuncCall):
        return _compile_func(node, field_map)
    if isinstance(node, Comparison):
        return _compile_comparison(node, field_map)
    raise FilterSyntaxError(f"Unsupported $filter AST node: {node!r}")


def parse_filter_to_predicate(
    filter_string: str | None, field_map: dict[str, str] | None = None
) -> Callable[[dict], bool]:
    """In-memory compiler for the shared $filter AST (see utils.filter_ast)."""
    if not filter_string:
        return lambda _row: True
    ast = parse_filter_ast(filter_string)
    try:
        return _compile(ast, field_map or {})
    except FilterSyntaxError:
        raise
    except Exception as exc:
        raise FilterSyntaxError(f"Could not compile $filter expression: {filter_string!r}") from exc
