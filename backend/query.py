"""Generic collection/single-entity read path: $filter/$orderby/$top/$skip/
$select/$expand orchestration, key-based lookup, ETag precondition
checking, CSRF/CORS gates, and the EditState synthetic-field stamping the
standard quick filter depends on. Depends on config.py, state.py,
odata_format.py, resolvers.py.
"""
from __future__ import annotations

from typing import Any, Dict, List, Optional, Tuple

import serve_config

from . import config
from . import state
from . import odata_format
from . import resolvers
from .patterns import Patterns

# Type aliases
EntityRow = Dict[str, Any]
KeyParts = Dict[str, Any]
QueryParams = Dict[str, str]


def _split_root_body(body: Optional[Dict[str, Any]]) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    """Возвращает (root_fields, basic_fields) — маршрутизация по BASIC_PROXY_FIELDS.
    
    Args:
        body: Request body dictionary
        
    Returns:
        Tuple of (root_fields, basic_fields) dictionaries
    """
    root_fields: Dict[str, Any] = {}
    basic_fields: Dict[str, Any] = {}
    for k, v in (body or {}).items():
        (basic_fields if k in config.BASIC_PROXY_FIELDS else root_fields)[k] = v
    return root_fields, basic_fields


def csrf_check_failed(headers: Dict[str, str]) -> bool:
    """Check if CSRF token validation failed.
    
    Args:
        headers: Request headers dictionary
        
    Returns:
        True if CSRF check failed, False otherwise
    """
    return headers.get("x-csrf-token") != config.CSRF_TOKEN


def _is_allowed_cors_origin(origin: Optional[str]) -> bool:
    """Check if origin is allowed for CORS.
    
    Args:
        origin: Origin header value
        
    Returns:
        True if origin is allowed, False otherwise
    """
    if not origin:
        return False
    return bool(
        Patterns.LOCAL_ORIGIN_RE.match(origin) or 
        Patterns.PREVIEW_ORIGIN_RE.match(origin)
    )


def _augment_edit_state_fields(rows: List[EntityRow]) -> List[EntityRow]:
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
                                             
    Args:
        rows: List of entity rows to augment
        
    Returns:
        Same list with augmented fields (mutated in place)
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
            row["DraftAdministrativeData/InProcessByUser"] = (
                draft_row.get("InProcessByUser", config.MOCK_USER) if has_draft else ""
            )
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


def _resolve_source(set_name: str) -> Tuple[Optional[List[EntityRow]], Optional[str]]:
    """Returns (data_list, entity_type) for a reference or transactional entity set, or (None, None).
    
    Args:
        set_name: Name of the entity set to resolve
        
    Returns:
        Tuple of (data_list, entity_type) or (None, None) if not found
    """
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


def _key_parts_of(set_name: str, row: EntityRow) -> KeyParts:
    """Extract key properties from entity row.
    
    Args:
        set_name: Entity set name to determine key properties
        row: Entity row to extract keys from
        
    Returns:
        Dictionary of key property names to values
    """
    props = config.KEY_PROP_TUPLES.get(set_name) or (serve_config.REFERENCE_KEY_PROPS.get(set_name),)
    return {p: row.get(p) for p in props}


def _parse_int_param(query: QueryParams, name: str, default: int) -> int:
    """Safe int(...) for $top/$skip - a malformed value (e.g. $top=abc, from
    a hand-typed URL or a client bug) must degrade to the default instead of
    raising an uncaught ValueError out of dispatch_request.
    
    Args:
        query: Query parameters dictionary
        name: Parameter name to parse
        default: Default value if parsing fails
        
    Returns:
        Parsed integer value or default
    """
    raw = query.get(name)
    if raw is None:
        return default
    try:
        return int(raw)
    except (TypeError, ValueError):
        return default


def _list_response(
    set_name: str, 
    query: QueryParams, 
    requesting_user: str = config.MOCK_USER
) -> Optional[Tuple[int, Dict[str, Any], str]]:
    """Build list response with filtering, ordering, and pagination.
    
    Args:
        set_name: Entity set name
        query: Query parameters dictionary
        requesting_user: User making the request (default: MOCK_USER)
        
    Returns:
        Tuple of (status_code, response_body, content_type) or None if not found
    """
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
        page_wrapped = [
            resolvers.apply_expand(r, set_name, query["$expand"], requesting_user) 
            for r in page_wrapped
        ]
    return (200, {"d": {"results": page_wrapped, "__count": str(len(filtered))}}, "application/json")


def _find_by_key(set_name: str, key_parts: KeyParts) -> Optional[EntityRow]:
    """Find entity by key properties.
    
    Args:
        set_name: Entity set name
        key_parts: Dictionary of key property names to values
        
    Returns:
        Entity row if found, None otherwise
    """
    if set_name in config.REFERENCE_DATA:
        code_field = serve_config.REFERENCE_KEY_PROPS[set_name]
        return next(
            (r for r in config.REFERENCE_DATA[set_name] 
             if str(r.get(code_field)) == str(key_parts[code_field])), 
            None
        )
    # [Фаза 5] DraftUUID != ZERO_GUID => явно адресованный черновик — читаем
    # из draft_store по его собственному ключу, не из store.
    if set_name == "CheckRoots":
        if key_parts.get("DraftUUID", config.ZERO_GUID) != config.ZERO_GUID:
            return state.draft_store["CheckRoots"].get(key_parts["DraftUUID"])
        return state.store["CheckRoots"].get(key_parts.get("ActiveUUID"))
    props = config.KEY_PROP_TUPLES[set_name]
    store_key = key_parts[props[0]] if len(props) == 1 else tuple(key_parts[p] for p in props)
    return state.store[set_name].get(store_key)


def _single_response(
    set_name: str, 
    key_parts: KeyParts, 
    query: QueryParams, 
    requesting_user: str = config.MOCK_USER
) -> Optional[Tuple[int, Dict[str, Any], str]]:
    """Build single entity response.
    
    Args:
        set_name: Entity set name
        key_parts: Dictionary of key property names to values
        query: Query parameters dictionary
        requesting_user: User making the request (default: MOCK_USER)
        
    Returns:
        Tuple of (status_code, response_body, content_type) or None if not found
    """
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


def _precondition_failed(
    set_name: str, 
    key_parts: KeyParts, 
    headers: Dict[str, str]
) -> bool:
    """Check if If-Match precondition failed for entity.
    
    Args:
        set_name: Entity set name
        key_parts: Dictionary of key property names to values
        headers: Request headers dictionary
        
    Returns:
        True if precondition failed, False otherwise
    """
    if set_name not in ("CheckRoots", "CheckBasics", "CheckItems", "Barriers"):
        return False
    if_match = headers.get("if-match", "")
    if not if_match or if_match == "*":
        return False
    row = _find_by_key(set_name, key_parts)
    if not row:
        return False
    # [Fix, browser-test pass] A draft-addressed CheckRoots key (DraftUUID !=
    # ZERO_GUID) skips the If-Match check entirely now - live-reproduced why
    # it must: the draft's aggregated etag (root + CheckBasics + this
    # draft's own tagged CheckItems/Barriers - see
    # odata_format.compute_etag_timestamp_ms) moves on EVERY child mutation
    # (add a CheckItem, edit a Barrier), not just root-field edits. The
    # client's locally-cached CheckRoots context has no way to learn about
    # that shift - a child create/update's HTTP response is scoped to the
    # CHILD's own resource, never the root's - so ANY root-field PATCH sent
    # after so much as one child-row edit in the same session carries a
    # stale If-Match and gets a false 412, even though nothing actually
    # conflicted. Concretely reproduced: create-draft, add a CheckItem, pick
    # its Type, then edit any root field - guaranteed 412, root fields never
    # persisted, confirmed via direct store inspection.
    #
    # Safe to skip: this branch only fires for DRAFT-addressed keys, and
    # UpdateHandler._update_root (the only caller for CheckRoots PATCH)
    # already re-checks draft ownership via draft_service._draft_owner_mismatch
    # immediately after this returns - that's the actual concurrency guard
    # for a draft (a draft is single-owner by construction, see
    # InProcessByUser), making If-Match redundant for it specifically. The
    # ACTIVE side (is_draft_addressed=False) keeps full If-Match protection
    # unchanged below - multi-user conflicts on a plain non-draft read/write
    # are a real scenario this mock still needs to catch.
    is_draft_addressed = (
        set_name == "CheckRoots" and key_parts.get("DraftUUID", config.ZERO_GUID) != config.ZERO_GUID
    )
    if is_draft_addressed:
        return False
    current_etag = (
        'W/"%s"' % row.get("LastChangedAt")
        if set_name != "CheckRoots"
        else 'W/"%s"' % odata_format._root_etag_string(row["RootId"])
    )
    return if_match != current_etag
