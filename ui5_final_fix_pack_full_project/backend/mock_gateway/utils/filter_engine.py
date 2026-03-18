from __future__ import annotations

import re
from datetime import datetime, timezone
from typing import Callable


_TOKEN_RE = re.compile(r"substringof\(|contains\(|datetime'[^']*'|/Date\([^)]*\)/|'[^']*'|\(|\)|,|\b(?:eq|ne|ge|le|and|or|true|false)\b|[A-Za-z_][A-Za-z0-9_]*", re.IGNORECASE)


def _literal(token: str):
    low = token.lower()
    if token.startswith("'") and token.endswith("'"):
        return token[1:-1]
    if low == "true":
        return True
    if low == "false":
        return False
    if low.startswith("datetime'"):
        return token[9:-1]
    return token


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


def parse_filter_to_predicate(filter_string: str | None, field_map: dict[str, str] | None = None) -> Callable[[dict], bool]:
    if not filter_string:
        return lambda _row: True
    tokens = _TOKEN_RE.findall(filter_string)
    i = 0
    fmap = field_map or {}

    def field_name(name: str) -> str:
        return fmap.get(name, name)

    def parse_expr():
        nonlocal i
        node = parse_term()
        while i < len(tokens) and tokens[i].lower() == "or":
            i += 1
            rhs = parse_term(); prev = node
            node = lambda row, a=prev, b=rhs: bool(a(row) or b(row))
        return node

    def parse_term():
        nonlocal i
        node = parse_factor()
        while i < len(tokens) and tokens[i].lower() == "and":
            i += 1
            rhs = parse_factor(); prev = node
            node = lambda row, a=prev, b=rhs: bool(a(row) and b(row))
        return node

    def parse_factor():
        nonlocal i
        if i < len(tokens) and tokens[i] == "(":
            i += 1
            node = parse_expr()
            if i < len(tokens) and tokens[i] == ")":
                i += 1
            return node
        return parse_predicate()

    def parse_predicate():
        nonlocal i
        tok = tokens[i]; low = tok.lower()
        if low in {"substringof(", "contains("}:
            i += 1
            needle = str(_literal(tokens[i])); i += 1
            if i < len(tokens) and tokens[i] == ",":
                i += 1
            fld = field_name(str(_literal(tokens[i]))); i += 1
            if i < len(tokens) and tokens[i] == ")":
                i += 1
            fn = lambda row, f=fld, n=needle.lower(): n in str(row.get(f, "")).lower()
            return fn

        fld = field_name(tok); i += 1
        if i + 1 >= len(tokens):
            return lambda _row: True
        op = tokens[i].lower(); i += 1
        rv = _literal(tokens[i]); i += 1

        def pred(row):
            lv = row.get(fld)
            if fld == "DateCheck":
                lvn = _date_only(lv); rvn = _date_only(rv)
                if op == "eq": return lvn == rvn
                if op == "ne": return lvn != rvn
                if op == "ge": return lvn >= rvn
                if op == "le": return lvn <= rvn
                return True
            if isinstance(rv, bool):
                lvb = bool(lv)
                return {"eq": lvb == rv, "ne": lvb != rv}.get(op, True)
            lvs = str(lv or "")
            rvs = str(rv or "")
            if op == "eq": return lvs == rvs
            if op == "ne": return lvs != rvs
            if op == "ge": return lvs >= rvs
            if op == "le": return lvs <= rvs
            return True

        return pred

    try:
        return parse_expr()
    except Exception:
        return lambda _row: True


def debug_explain(filter_string: str) -> str:
    return f"TOKENS={_TOKEN_RE.findall(filter_string or '')}"
