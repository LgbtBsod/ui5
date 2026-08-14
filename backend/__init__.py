"""Mock OData v2 + static-file server, split into single-responsibility
modules (see each module's own docstring). Import order below mirrors the
acyclic dependency chain:

    config -> state -> odata_format -> resolvers -> draft_service
                                                  -> query -> crud
    {resolvers, draft_service, query, crud} -> dispatch -> batch -> http_server
                                                            seed (standalone)
    db_models, error_format -> (used by dispatch, draft_service, resolvers)

Everything re-exported here is what redux/serve.py (the thin backwards-
compatible shim + process entry point) exposes as `serve.XXX`, matching the
flat single-module surface tests/test_serve.py already relies on.
"""
from .config import (
    DEFAULT_PORT,
    ODATA_PREFIX,
    WEBAPP_DIR,
    CSRF_TOKEN,
    REFERENCE_DATA,
    DATASET_INDEX,
    ZERO_GUID,
    MOCK_USER,
    MOCK_USER_REGISTRY,
    MOCK_USER_DISPLAY_NAME,
    KEY_PROP_TUPLES,
    ENTITY_TYPES,
    BASIC_PROXY_FIELDS,
    ETAG_SENTINEL_MS,
    REQUIRED_ROOT_FIELDS,
    REQUIRED_FIELD_ALTERNATES,
    REQUIRED_CHILD_FIELD,
    REQUIRED_CHILD_FIELD_LABEL,
)
from .state import (
    store,
    draft_store,
    next_doc_id,
    draw_doc_id,
    _set_sap_message,
    _pop_sap_message,
    _set_pending_etag,
    _pop_pending_etag,
    pop_pending_headers,
    _resolve_mock_user,
    _mock_user_display_name,
    _find_draft_by_active_uuid,
    _checks_of,
    _barriers_of,
)
from .odata_format import (
    odata_date,
    parse_odata_date,
    generate_uuid,
    _build_key_segment,
    meta_for,
    _checkroot_meta,
    compute_etag_timestamp_ms,
    _root_etag_string,
)
from .error_format import (
    format_error_response,
    format_validation_error,
    format_child_validation_error,
    format_draft_locked_error,
    format_not_found_error,
    format_csrf_error,
    format_internal_error,
)
from .resolvers import (
    _ref_lookup,
    resolve_check_basic,
    _resolve_type_and_result,
    resolve_check_item,
    resolve_barrier,
    _rate_to_criticality,
    compute_check_root_view,
    wrap_entity,
    _resolve_sublocation_names,
    _rewrite_with_sublocation_tokens,
    compare_dates,
    eval_one_filter,
    apply_filter,
    apply_select,
    apply_orderby,
    latest_location_version_per_code,
    _draft_admin_data_row,
    _sibling_row,
    apply_expand,
)
from .draft_service import (
    _draft_owner_mismatch,
    _missing_required_fields,
    _snapshot_children,
    _restore_children,
    _purge_children,
    _clone_children_into_draft,
    _purge_tagged_children,
    _activate_tagged_children,
    _draft_prepare,
    _draft_activate,
    _draft_discard,
)
from .query import (
    _split_root_body,
    csrf_check_failed,
    _is_allowed_cors_origin,
    _augment_edit_state_fields,
    _resolve_source,
    _key_parts_of,
    _parse_int_param,
    _list_response,
    _find_by_key,
    _single_response,
    _precondition_failed,
)
from .crud import (
    _create_check_basic,
    _create_check_item,
    _create_barrier,
)
from .patterns import (
    SINGLE_KEY_RE,
    COMPOSITE_KEY_RE,
    CHECKROOT_KEY_RE,
    NAV_COLLECTION_RE,
    NAV_CREATE_RE,
    NAV_DRAFT_ADMIN_RE,
    NAV_SIBLING_RE,
)
from .dispatch import (
    _nav_root_id,
    _parse_key,
    dispatch_request,
)
from .batch import (
    split_on_boundary,
    parse_embedded_http,
    parse_batch_part,
    build_response_part,
    handle_batch,
)
from .http_server import (
    read_static_file,
    _requote_query,
    FioriHandler,
    print_banner,
    main,
    USAGE,
)
from .seed import seed_test_data

# [Fix] Explicit __all__ so `from backend import *` (used by serve.py's thin
# shim) actually re-exports the underscore-prefixed names too - Python's
# default `import *` behavior silently drops any name starting with "_",
# which is most of this surface (it was written as an internal single
# module originally, not a public API). Keeping this list in exact sync with
# the imports above is what tests/test_serve.py's `serve.XXX` attribute
# access depends on.
__all__ = [
    "DEFAULT_PORT", "ODATA_PREFIX", "WEBAPP_DIR", "CSRF_TOKEN", "REFERENCE_DATA",
    "DATASET_INDEX", "ZERO_GUID", "MOCK_USER", "MOCK_USER_REGISTRY",
    "MOCK_USER_DISPLAY_NAME", "KEY_PROP_TUPLES", "ENTITY_TYPES", "BASIC_PROXY_FIELDS",
    "ETAG_SENTINEL_MS", "REQUIRED_ROOT_FIELDS", "REQUIRED_FIELD_ALTERNATES",
    "REQUIRED_CHILD_FIELD", "REQUIRED_CHILD_FIELD_LABEL",
    "store", "draft_store", "next_doc_id", "draw_doc_id", "_set_sap_message",
    "_pop_sap_message", "_set_pending_etag", "_pop_pending_etag", "pop_pending_headers",
    "_resolve_mock_user",
    "_mock_user_display_name", "_find_draft_by_active_uuid", "_checks_of", "_barriers_of",
    "odata_date", "parse_odata_date", "generate_uuid", "_build_key_segment", "meta_for",
    "_checkroot_meta", "compute_etag_timestamp_ms", "_root_etag_string",
    "format_error_response", "format_validation_error", "format_child_validation_error",
    "format_draft_locked_error", "format_not_found_error", "format_csrf_error",
    "format_internal_error",
    "_ref_lookup", "resolve_check_basic", "_resolve_type_and_result", "resolve_check_item",
    "resolve_barrier", "_rate_to_criticality", "compute_check_root_view", "wrap_entity",
    "_resolve_sublocation_names", "_rewrite_with_sublocation_tokens", "compare_dates",
    "eval_one_filter", "apply_filter", "apply_select", "apply_orderby", "latest_location_version_per_code",
    "_draft_admin_data_row", "_sibling_row", "apply_expand",
    "_draft_owner_mismatch",
    "_missing_required_fields", "_snapshot_children", "_restore_children", "_purge_children",
    "_clone_children_into_draft", "_purge_tagged_children", "_activate_tagged_children",
    "_draft_prepare", "_draft_activate", "_draft_discard",
    "_split_root_body", "csrf_check_failed", "_is_allowed_cors_origin",
    "_augment_edit_state_fields", "_resolve_source", "_key_parts_of", "_parse_int_param",
    "_list_response", "_find_by_key", "_single_response", "_precondition_failed",
    "_create_check_basic", "_create_check_item", "_create_barrier",
    "SINGLE_KEY_RE", "COMPOSITE_KEY_RE", "CHECKROOT_KEY_RE", "NAV_COLLECTION_RE",
    "NAV_CREATE_RE", "NAV_DRAFT_ADMIN_RE", "NAV_SIBLING_RE", "_nav_root_id", "_parse_key",
    "dispatch_request",
    "split_on_boundary", "parse_embedded_http", "parse_batch_part", "build_response_part",
    "handle_batch",
    "read_static_file", "_requote_query", "FioriHandler", "print_banner", "main", "USAGE",
    "seed_test_data",
]
