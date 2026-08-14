"""Generic, non-draft CRUD engine for OData v2 mock services - list/get/
create/update/delete driven entirely by EntityConfig, no per-entity-set
code required. This is the toolkit half that replaces hand-rolled dispatch
functions like demo_service/dispatch.py's original 62 lines.

Intentionally minimal - no $filter/$orderby/$select/$expand support. That
machinery (backend/resolvers.py's apply_filter/apply_select/apply_orderby)
was written against CheckRoots' own filter vocabulary and wasn't confirmed
generic during the toolkit design review, so it stays out of scope here
rather than being silently half-ported (see the design plan's "no silent
caps" note). A service needing query support can layer it on top of the
plain list this returns, or roll its own - this only covers list/single/
create/update/delete, matching what demo_service actually needed plus
update/delete for services that need real mutation, not just a read-mostly
demo.

Also no draft-protocol awareness at all - see toolkit/draft.py for that.
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any, Callable, Dict, Optional, Tuple
from urllib.parse import unquote

from .patterns import ENTITY_SET_RE, SINGLE_KEY_RE

#: Composite key entity: EntityName(Key1='val1',Key2='val2',...) - generic
#: over ANY key prop names, unlike backend.patterns.COMPOSITE_KEY_RE which
#: is hardcoded to RootId/ItemId|BarrierId.
_COMPOSITE_KEY_RE = re.compile(r"^(\w+)\(([^)]*)\)$")
_KEY_PART_RE = re.compile(r"(\w+)='([^']*)'")


@dataclass(frozen=True)
class EntityConfig:
    """One entity set's shape, as far as generic CRUD needs to know it.

    entity_set:  store dict key / OData EntitySet name, e.g. "Widgets".
    entity_type: OData EntityType name for __metadata.type, e.g.
                 "ZDemoService.Widget".
    key_props:   the store's key shape - a single prop name ("Id",) for a
                 plain dict[str, row] store, or several ("RootId", "ItemId")
                 for a composite dict[tuple, row] store (tuple order must
                 match key_props order).
    url_prefix:  this service's OData root, e.g.
                 "/sap/opu/odata/sap/ZDEMO_SRV" - baked into every
                 __metadata.uri. backend/config.py's ODATA_PREFIX plays the
                 same role for ZCHECK_SRV but is hardcoded to it, so it
                 can't be reused for a second service.
    id_factory:  optional zero-arg callable that generates a value for the
                 FIRST key prop on create when the client didn't supply one
                 (e.g. demo_service.state.draw_id) - only consulted for
                 single-key configs; composite-key creates must supply every
                 key part in the request body themselves (they're usually
                 addressing an already-known parent key, not inventing one).
    """
    entity_set: str
    entity_type: str
    key_props: Tuple[str, ...]
    url_prefix: str
    id_factory: Optional[Callable[[], str]] = None


def _store_key(key_props: Tuple[str, ...], key_parts: Dict[str, str]):
    """Single prop -> bare string (plain dict[str, row] store); multiple
    props -> tuple in key_props order (composite dict[tuple, row] store)."""
    if len(key_props) == 1:
        return key_parts[key_props[0]]
    return tuple(key_parts[prop] for prop in key_props)


def _parse_key(path: str, key_props: Tuple[str, ...]) -> Optional[Tuple[str, Dict[str, str]]]:
    """Returns (entity_set, key_parts) if `path` is a keyed segment matching
    this config's key shape, else None (caller tries other configs / the
    plain entity-set-list match instead)."""
    if len(key_props) == 1:
        m = SINGLE_KEY_RE.match(path)
        if not m:
            return None
        return m.group(1), {key_props[0]: unquote(m.group(2))}
    m = _COMPOSITE_KEY_RE.match(path)
    if not m:
        return None
    entity_set, raw_parts = m.group(1), m.group(2)
    parts = dict(_KEY_PART_RE.findall(raw_parts))
    if set(parts) != set(key_props):
        return None
    return entity_set, {k: unquote(v) for k, v in parts.items()}


def _key_segment(entity_set: str, key_props: Tuple[str, ...], row: Dict[str, Any]) -> str:
    if len(key_props) == 1:
        return "%s('%s')" % (entity_set, row[key_props[0]])
    parts = ",".join("%s='%s'" % (p, row[p]) for p in key_props)
    return "%s(%s)" % (entity_set, parts)


def _wrap(config: EntityConfig, row: Dict[str, Any]) -> Dict[str, Any]:
    out = dict(row)
    out["__metadata"] = {
        "type": config.entity_type,
        "uri": "%s/%s" % (config.url_prefix, _key_segment(config.entity_set, config.key_props, row)),
    }
    return out


def _not_found() -> Tuple[int, Any, str]:
    return 404, {"error": {"message": {"value": "Not found"}}}, "application/json"


def dispatch_request(
    configs: Dict[str, EntityConfig],
    store: Dict[str, Dict[Any, Dict[str, Any]]],
    method: str,
    rel_url: str,
    body: Optional[dict] = None,
    headers: Optional[dict] = None,
) -> Tuple[int, Any, str]:
    """Generic dispatch for a set of non-draft entities, matching the
    gateway's plugin dispatch_request contract (see gateway/registry.py).

    `configs` maps EntitySet name -> its EntityConfig. `store` maps
    EntitySet name -> its own dict (single-key: {key: row}; composite-key:
    {(k1, k2, ...): row}) - same shape convention as backend/state.py's
    `store`, just a separate dict per service.
    """
    path = rel_url.split("?", 1)[0]
    method = method.upper()

    if method == "GET" and path == "":
        return 200, {"d": {"EntitySets": sorted(configs)}}, "application/json"

    m = ENTITY_SET_RE.match(path)
    if m and m.group(1) in configs:
        config = configs[m.group(1)]
        rows = store.setdefault(config.entity_set, {})
        if method == "GET":
            results = [_wrap(config, row) for row in rows.values()]
            return 200, {"d": {"results": results, "__count": str(len(results))}}, "application/json"
        if method == "POST":
            row = dict(body or {})
            if len(config.key_props) == 1 and not row.get(config.key_props[0]) and config.id_factory:
                row[config.key_props[0]] = config.id_factory()
            missing = [p for p in config.key_props if not row.get(p)]
            if missing:
                return 400, {"error": {"message": {"value": "Missing key property '%s'" % missing[0]}}}, "application/json"
            key = _store_key(config.key_props, {p: row[p] for p in config.key_props})
            rows[key] = row
            return 201, {"d": _wrap(config, row)}, "application/json"

    for config in configs.values():
        parsed = _parse_key(path, config.key_props)
        if parsed is None or parsed[0] != config.entity_set:
            continue
        _, key_parts = parsed
        rows = store.setdefault(config.entity_set, {})
        key = _store_key(config.key_props, key_parts)
        if method == "GET":
            row = rows.get(key)
            return (200, {"d": _wrap(config, row)}, "application/json") if row is not None else _not_found()
        if method in ("PATCH", "PUT", "MERGE"):
            row = rows.get(key)
            if row is None:
                return _not_found()
            row.update(body or {})
            return 204, None, "application/json"
        if method == "DELETE":
            if key not in rows:
                return _not_found()
            del rows[key]
            return 204, None, "application/json"
        return _not_found()

    return _not_found()
