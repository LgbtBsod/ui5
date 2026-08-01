"""Generic collection/single-entity read path: $filter/$orderby/$top/$skip/
$select/$expand orchestration, key-based lookup, ETag precondition
checking, CSRF/CORS gates, and the EditState synthetic-field stamping the
standard quick filter depends on. Depends on config.py, state.py,
odata_format.py, resolvers.py.
"""
import re

import serve_config

from . import config
from . import state
from . import odata_format
from . import resolvers


def _split_root_body(body):
    """Возвращает (root_fields, basic_fields) — маршрутизация по BASIC_PROXY_FIELDS."""
    root_fields, basic_fields = {}, {}
    for k, v in (body or {}).items():
        (basic_fields if k in config.BASIC_PROXY_FIELDS else root_fields)[k] = v
    return root_fields, basic_fields


def csrf_check_failed(headers):
    return headers.get("x-csrf-token") != config.CSRF_TOKEN


_LOCAL_ORIGIN_RE = re.compile(r"^https?://(localhost|127\.0\.0\.1)(:\d+)?$", re.I)
_PREVIEW_ORIGIN_RE = re.compile(r"^https://[a-z0-9-]+\.space-z\.ai$", re.I)


def _is_allowed_cors_origin(origin):
    if not origin:
        return False
    return bool(_LOCAL_ORIGIN_RE.match(origin) or _PREVIEW_ORIGIN_RE.match(origin))


def _augment_edit_state_fields(rows):
    """Stamps two synthetic, dict-key-as-nav-path fields onto each CheckRoots
    row so the generic $filter engine (resolvers.apply_filter/eval_one_filter)
    can treat them exactly like any other field - no special nav-property
    resolution needed there, since eval_one_filter's field-name pattern
    already allows "/" (see FIELD_RE). These are exactly what the standard
    Fiori Elements EditState quick filter (see annotations.xml) sends as
    $filter clauses:

      SiblingEntity/IsActiveEntity        - the OTHER half of an active/draft
                                             pair, if one exists; None if this
                                             row has no sibling at all.
      DraftAdministrativeData/InProcessByUser - who's editing the draft half
                                             of the pair - the draft's OWN
                                             InProcessByUser (see
                                             draft_service._draft_prepare/
                                             X-Mock-User), not a hardcoded
                                             constant, now that more than one
                                             identity can plausibly hold a
                                             draft. Empty string when there's
                                             no draft at all (an active row
                                             with no edit in progress).
    """
    for row in rows:
        root_id = row["RootId"]
        is_active = bool(row.get("IsActiveEntity", True))
        # Seed/legacy active rows may never have had IsActiveEntity stamped
        # on them at all (it defaults to True by convention rather than being
        # written at creation) - normalize it explicitly here so the $filter
        # engine's plain row.get("IsActiveEntity") (no default) sees the
        # right value instead of None.
        row["IsActiveEntity"] = is_active
        if is_active:
            _draft_uuid, draft_row = state._find_draft_by_active_uuid(root_id)
            has_draft = draft_row is not None
            row["SiblingEntity/IsActiveEntity"] = False if has_draft else None
            row["DraftAdministrativeData/InProcessByUser"] = draft_row.get("InProcessByUser", config.MOCK_USER) if has_draft else ""
            # HasDraftEntity/HasActiveEntity are otherwise only computed later
            # in wrap_entity - which runs AFTER filtering - so the $filter
            # engine would never see them without stamping here too (e.g. the
            # EditState "Unchanged" option filters on HasDraftEntity eq false).
            row["HasDraftEntity"] = has_draft
            row["HasActiveEntity"] = True
        else:
            has_active_sibling = row.get("ActiveUUID", config.ZERO_GUID) != config.ZERO_GUID
            row["SiblingEntity/IsActiveEntity"] = True if has_active_sibling else None
            row["DraftAdministrativeData/InProcessByUser"] = row.get("InProcessByUser", config.MOCK_USER)
            row["HasDraftEntity"] = False
            row["HasActiveEntity"] = has_active_sibling
    return rows


def _resolve_source(set_name):
    """Returns (data_list, entity_type) for a reference or transactional entity set, or (None, None)."""
    if set_name in config.REFERENCE_DATA:
        entity_type, _key_prop = config.DATASET_INDEX[set_name]
        return config.REFERENCE_DATA[set_name], entity_type
    if set_name in state.store:
        entity_type = config.ENTITY_TYPES.get(set_name, set_name)
        if set_name == "CheckRoots":
            data = [resolvers.compute_check_root_view(rid) for rid in state.store["CheckRoots"]]
            # [Fix] Drafts (both orphan create-drafts with no active twin, and
            # edit-drafts of an existing active row) live only in draft_store
            # and were invisible from the List Report - there was no way to
            # find/resume/search them, and the standard EditState quick filter
            # (see annotations.xml Common.DraftRoot/SemanticKey) has no rows to
            # select from without them. Real BOPF draft services return BOTH
            # the active row and its draft row from the same collection query
            # and let the client-side EditState filter decide which to show
            # (e.g. "All" = prefer draft over its active twin, see
            # _augment_edit_state_fields/resolvers.apply_filter) - so we
            # surface every draft_store row here too, not just orphans.
            # wrap_entity's draft branch (row["IsActiveEntity"] is already
            # False on these) recomputes the view via
            # compute_check_root_view(root_id, root_override=row).
            data = data + list(state.draft_store["CheckRoots"].values())
            _augment_edit_state_fields(data)
        else:
            data = list(state.store[set_name].values())
        return data, entity_type
    return None, None


def _key_parts_of(set_name, row):
    props = config.KEY_PROP_TUPLES.get(set_name) or (serve_config.REFERENCE_KEY_PROPS.get(set_name),)
    return {p: row.get(p) for p in props}


def _parse_int_param(query, name, default):
    """Safe int(...) for $top/$skip - a malformed value (e.g. $top=abc, from
    a hand-typed URL or a client bug) must degrade to the default instead of
    raising an uncaught ValueError out of dispatch_request."""
    raw = query.get(name)
    if raw is None:
        return default
    try:
        return int(raw)
    except (TypeError, ValueError):
        return default


def _list_response(set_name, query, requesting_user=config.MOCK_USER):
    data, entity_type = _resolve_source(set_name)
    if data is None:
        return None
    filtered = resolvers.apply_filter(data, query.get("$filter"), query.get("search"))
    filtered = resolvers.apply_orderby(filtered, query.get("$orderby"))
    skip = _parse_int_param(query, "$skip", 0)
    top = _parse_int_param(query, "$top", len(filtered))
    page = filtered[skip:skip + top]
    page_selected = resolvers.apply_select(page, query.get("$select"))
    page_wrapped = [
        resolvers.wrap_entity(set_name, entity_type, raw, _key_parts_of(set_name, raw))
        for raw, selected in zip(page, page_selected)
    ]
    if query.get("$select"):
        keep = set(f.strip() for f in query["$select"].split(",")) | {"__metadata"}
        page_wrapped = [{k: v for k, v in r.items() if k in keep} for r in page_wrapped]
    if query.get("$expand"):
        page_wrapped = [resolvers.apply_expand(r, set_name, query["$expand"], requesting_user) for r in page_wrapped]
    return (200, {"d": {"results": page_wrapped, "__count": str(len(filtered))}}, "application/json")


def _find_by_key(set_name, key_parts):
    if set_name in config.REFERENCE_DATA:
        code_field = serve_config.REFERENCE_KEY_PROPS[set_name]
        return next((r for r in config.REFERENCE_DATA[set_name] if str(r.get(code_field)) == str(key_parts[code_field])), None)
    # [Фаза 5] DraftUUID != ZERO_GUID => явно адресованный черновик — читаем
    # из draft_store по его собственному ключу, не из store.
    if set_name == "CheckRoots":
        if key_parts.get("DraftUUID", config.ZERO_GUID) != config.ZERO_GUID:
            return state.draft_store["CheckRoots"].get(key_parts["DraftUUID"])
        return state.store["CheckRoots"].get(key_parts.get("ActiveUUID"))
    props = config.KEY_PROP_TUPLES[set_name]
    store_key = key_parts[props[0]] if len(props) == 1 else tuple(key_parts[p] for p in props)
    return state.store[set_name].get(store_key)


def _single_response(set_name, key_parts, query, requesting_user=config.MOCK_USER):
    if set_name in config.REFERENCE_DATA:
        entity_type = config.DATASET_INDEX[set_name][0]
    elif set_name in state.store:
        entity_type = config.ENTITY_TYPES.get(set_name, set_name)
    else:
        return None
    row = _find_by_key(set_name, key_parts)
    if not row:
        return (404, {"error": {"message": {"value": "Not found"}}}, "application/json")
    wrapped = resolvers.wrap_entity(set_name, entity_type, row, key_parts)
    if query.get("$expand"):
        wrapped = resolvers.apply_expand(wrapped, set_name, query["$expand"], requesting_user)
    return (200, {"d": wrapped}, "application/json")


def _precondition_failed(set_name, key_parts, headers):
    if set_name not in ("CheckRoots", "CheckBasics", "CheckItems", "Barriers"):
        return False
    if_match = headers.get("if-match", "")
    if not if_match or if_match == "*":
        return False
    row = _find_by_key(set_name, key_parts)
    if not row:
        return False
    # [Fix] A draft-addressed CheckRoots key (DraftUUID != ZERO_GUID) must be
    # compared against the DRAFT's own aggregated etag (root + CheckBasics +
    # this draft's own tagged CheckItems/Barriers - see
    # odata_format.compute_etag_timestamp_ms) - exactly what wrap_entity
    # hands the client as __metadata.etag for that same row. Comparing
    # against the ACTIVE side's etag instead meant the very first legitimate
    # PATCH to a draft made every subsequent PATCH with a freshly-fetched
    # If-Match permanently fail with a false 412, since the active-side
    # timestamp the check compared against never moved.
    is_draft_addressed = (
        set_name == "CheckRoots" and key_parts.get("DraftUUID", config.ZERO_GUID) != config.ZERO_GUID
    )
    if set_name != "CheckRoots":
        current_etag = 'W/"%s"' % row.get("LastChangedAt")
    elif is_draft_addressed:
        current_etag = 'W/"%s"' % odata_format._root_etag_string(row["RootId"], root_override=row)
    else:
        current_etag = 'W/"%s"' % odata_format._root_etag_string(row["RootId"])
    return if_match != current_etag
