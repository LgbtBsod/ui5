"""Shared AST + tokenizer for OData $filter expressions.

Single source of truth for $filter grammar. Previously duplicated almost identically
across utils/odata_filter_sql.py (SQL predicate builder) and utils/filter_engine.py
(in-memory row predicate builder), with each independently maintaining its own
tokenizer/parser and drifting apart in operator/function coverage (e.g. the SQL side
didn't support startswith() or decimal literals, the in-memory side tokenized
startswith( but never actually handled it in the grammar). Parsing happens exactly once
here, producing an AST; each backend only has to know how to *compile* that AST into its
own representation (SQLAlchemy expression vs. a Python predicate closure), not how to
*parse* $filter syntax.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Union

from utils.filter_errors import FilterSyntaxError
from utils.patterns import ODATA_COMPARISON_OPS, ODATA_FILTER_TOKEN_PATTERN, ODATA_FUNCTION_TOKENS

# Re-export for backward compatibility if needed
_COMPARISON_OPS = ODATA_COMPARISON_OPS
_FUNCTION_TOKENS = ODATA_FUNCTION_TOKENS
_TOKEN_RE = ODATA_FILTER_TOKEN_PATTERN


@dataclass(frozen=True)
class Comparison:
    field: str
    op: str
    value: object


@dataclass(frozen=True)
class FuncCall:
    name: str  # "substringof" / "contains" / "startswith"
    field: str
    value: object


@dataclass(frozen=True)
class BoolOp:
    op: str  # "and" / "or"
    left: "ASTNode"
    right: "ASTNode"


@dataclass(frozen=True)
class Not:
    operand: "ASTNode"


ASTNode = Union[Comparison, FuncCall, BoolOp, Not]


def _parse_literal(token: str):
    if token.startswith("'") and token.endswith("'"):
        return token[1:-1]
    low = token.lower()
    if low == "true":
        return True
    if low == "false":
        return False
    if low.startswith("datetime'") and token.endswith("'"):
        return token[9:-1]
    if token.startswith("/Date(") and token.endswith(")/"):
        return token
    if re.fullmatch(r"-?\d+", token):
        return int(token)
    if re.fullmatch(r"-?\d+\.\d+", token):
        return float(token)
    return token


def parse_filter_ast(filter_string: str | None) -> ASTNode | None:
    """Parse an OData $filter expression into an AST. Returns None for an empty filter.

    Raises FilterSyntaxError for anything unparseable, an unknown operator, or an
    unbalanced/incomplete expression - callers must fail closed (HTTP 400), never
    silently fall back to "match everything".
    """
    if not filter_string:
        return None
    tokens = _TOKEN_RE.findall(filter_string)
    if not tokens:
        raise FilterSyntaxError(f"Empty/unrecognized $filter expression: {filter_string!r}")
    idx = 0

    def parse_expr():
        nonlocal idx
        node = parse_term()
        while idx < len(tokens) and tokens[idx].lower() == "or":
            idx += 1
            node = BoolOp("or", node, parse_term())
        return node

    def parse_term():
        nonlocal idx
        node = parse_factor()
        while idx < len(tokens) and tokens[idx].lower() == "and":
            idx += 1
            node = BoolOp("and", node, parse_factor())
        return node

    def parse_factor():
        nonlocal idx
        if idx < len(tokens) and tokens[idx].lower() == "not":
            idx += 1
            return Not(parse_factor())
        if idx < len(tokens) and tokens[idx] == "(":
            idx += 1
            node = parse_expr()
            if idx < len(tokens) and tokens[idx] == ")":
                idx += 1
            else:
                raise FilterSyntaxError("Unbalanced parentheses in $filter")
            return node
        return parse_predicate()

    def parse_predicate():
        nonlocal idx
        if idx >= len(tokens):
            raise FilterSyntaxError("Unexpected end of $filter expression")
        tok = tokens[idx]
        low = tok.lower()
        if low in _FUNCTION_TOKENS:
            idx += 1
            if idx >= len(tokens):
                raise FilterSyntaxError(f"Unterminated {tok}")
            first = _parse_literal(tokens[idx])
            idx += 1
            if idx < len(tokens) and tokens[idx] == ",":
                idx += 1
            if idx >= len(tokens):
                raise FilterSyntaxError(f"Unterminated {tok}")
            second = _parse_literal(tokens[idx])
            idx += 1
            if idx < len(tokens) and tokens[idx] == ")":
                idx += 1
            else:
                raise FilterSyntaxError(f"Unterminated {tok}")
            name = low[:-1]  # strip trailing "("
            if name == "substringof":
                needle, field = first, second
            else:
                field, needle = first, second
            node: ASTNode = FuncCall(name, str(field), needle)
            # SQL-only extension carried over from the legacy SQL parser:
            # contains(Field,'x') eq true/false negates the match.
            if idx + 1 < len(tokens) and tokens[idx].lower() == "eq" and tokens[idx + 1].lower() in {"true", "false"}:
                negate = tokens[idx + 1].lower() == "false"
                idx += 2
                if negate:
                    node = Not(node)
            return node

        field = tok
        idx += 1
        if idx >= len(tokens):
            raise FilterSyntaxError(f"Incomplete comparison for field {field!r}")
        op = tokens[idx].lower()
        idx += 1
        if op not in _COMPARISON_OPS:
            raise FilterSyntaxError(f"Unsupported operator {op!r}")
        if idx >= len(tokens):
            raise FilterSyntaxError(f"Missing comparison value for field {field!r}")
        value = _parse_literal(tokens[idx])
        idx += 1
        return Comparison(field, op, value)

    try:
        node = parse_expr()
    except FilterSyntaxError:
        raise
    except (IndexError, ValueError) as exc:
        raise FilterSyntaxError(f"Could not parse $filter expression: {filter_string!r}") from exc

    if idx != len(tokens):
        raise FilterSyntaxError(f"Unexpected trailing tokens in $filter: {filter_string!r}")
    return node
