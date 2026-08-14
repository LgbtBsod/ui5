"""All mutable, process-lifetime state for the mock server, plus the small
set of accessors tightly coupled to it (mailboxes, mock-user resolution,
raw child-row lookups). Everything here is safe for any other module in the
package to import - state.py itself only depends on config.py.
"""
from typing import Any, Dict, List

import json

import serve_config

from . import config

# Type aliases for store structures
EntityKey = str  # RootId for CheckRoots/CheckBasics
CompositeKey = tuple  # (RootId, ItemId) or (RootId, BarrierId)
EntityRow = Dict[str, Any]
EntitySet = Dict[EntityKey, EntityRow]
CompositeEntitySet = Dict[CompositeKey, EntityRow]

# store["CheckRoots"][RootId] -> dict ; store["CheckBasics"][RootId] -> dict
# store["CheckItems"][(RootId, ItemId)] -> dict ; store["Barriers"][(RootId, BarrierId)] -> dict
store: Dict[str, EntitySet | CompositeEntitySet] = {set_name: {} for set_name in serve_config.TRANSACTIONAL_ENTITIES}

# [Фаза 5, NW 7.50/pre-S4 1610] Параллельный слой черновиков — ТОЛЬКО
# CheckRoots (см. redux/abap/README.md). Ключ — DraftUUID (собственный,
# отличный от RootId для edit-драфта существующей активной записи; равен
# RootId для create-драфта — см. config.ZERO_GUID и _find_draft_by_active_uuid).
# Композиция (CheckBasics/CheckItems/Barriers) в draft-режиме не участвует —
# активация/чтение черновика видит их как в активной записи. Отдельный
# словарь, а не поле внутри store["CheckRoots"], чтобы не трогать формат
# ключей store для уже работающих non-draft путей (zero regression risk).
draft_store: Dict[str, EntitySet] = {"CheckRoots": {}}

next_doc_id: List[int] = [1]


def draw_doc_id() -> str:
    """Generate next document ID with zero-padding."""
    doc_id = str(next_doc_id[0]).zfill(10)
    next_doc_id[0] += 1
    return doc_id


# [Fix] sap-message on draft Activation. Standard Fiori Elements shows the
# "object saved" success toast by reading the `sap-message` response header
# (SAP Gateway convention) - this mock never emitted one, so Save/Activate
# always looked like a no-op even when it fully succeeded. HTTPServer here is
# single-threaded (no ThreadingMixIn), so a plain module-level "mailbox" set
# right before returning from dispatch_request and popped by the HTTP layer
# right after is safe - no concurrent request can interleave and steal it.
_pending_sap_message = None


def _set_sap_message(message, severity="success", code=""):
    global _pending_sap_message
    # [Fix] Must NOT be percent-encoded. sap.ui.model.odata.ODataMessageParser
    # ._parseHeader calls JSON.parse() directly on the raw header string -
    # confirmed by reading the actual 1.71.84 sap/ui/core source - there is no
    # decodeURIComponent() step anywhere before that. A percent-encoded
    # payload ("%7B%22code%22...") is not valid JSON, so JSON.parse() throws,
    # the parser logs an error and returns without adding anything to the
    # MessageManager - the header was being silently dropped client-side the
    # whole time. HTTP header values must still be Latin-1/ASCII-safe though
    # (BaseHTTPRequestHandler.send_header can't take raw Cyrillic), so use
    # ensure_ascii=True instead: it \uXXXX-escapes non-ASCII text inside the
    # JSON string, which is pure ASCII and *is* valid JSON, so JSON.parse()
    # decodes it back to the original Cyrillic text with zero client-side
    # decoding step required - exactly the real SAP Gateway convention.
    _pending_sap_message = json.dumps({"code": code, "message": message, "severity": severity}, ensure_ascii=True)


def _pop_sap_message():
    global _pending_sap_message
    msg = _pending_sap_message
    _pending_sap_message = None
    return msg


# [Fix] ETag response header on PATCH/MERGE/PUT. sap.ui.model.odata.v2.
# ODataModel only refreshes its cached entity etag (used to build the NEXT
# request's If-Match) from two places: the response BODY's __metadata.etag
# (only present on GET/POST, since PATCH/MERGE correctly return 204 with no
# body), or the response's HTTP `ETag` header (_updateETag, called for every
# request incl. 204s). This server computed a fresh etag server-side on every
# PATCH (compute_etag_timestamp_ms bumps on every field edit) but never sent
# it back - so after a SECOND inline field edit within the same draft
# session, the client still had the STALE etag from the initial GET, sent it
# as If-Match, and got a false 412 Precondition Failed. Same single-threaded
# "mailbox" pattern as _pending_sap_message (safe for the same reason - one
# request in flight at a time).
_pending_etag = None


def _set_pending_etag(etag):
    global _pending_etag
    _pending_etag = etag


def _pop_pending_etag():
    global _pending_etag
    etag = _pending_etag
    _pending_etag = None
    return etag


def pop_pending_headers() -> Dict[str, str]:
    """[Fix, gateway pass] Uniform "mailbox" hand-off for the multi-service
    gateway (see gateway/registry.py's ServicePlugin.pop_pending_headers) -
    wraps the two existing single-purpose mailboxes above into one dict the
    gateway's HTTP layer can apply generically without knowing which header
    names any given plugin cares about. Purely additive: _pop_sap_message/
    _pop_pending_etag and their existing direct callers (http_server.py)
    are unchanged - this just offers a second, uniform way to drain the
    same state for callers that don't want to know the specific header
    names in advance."""
    headers: Dict[str, str] = {}
    msg = _pop_sap_message()
    if msg:
        headers["sap-message"] = msg
    etag = _pop_pending_etag()
    if etag:
        headers["ETag"] = etag
    return headers


def _resolve_mock_user(headers):
    uname = (headers or {}).get("x-mock-user")
    uname = (uname or "").strip()
    return uname or config.MOCK_USER


def _mock_user_display_name(uname):
    return config.MOCK_USER_REGISTRY.get(uname, uname)


def _find_draft_by_active_uuid(active_uuid):
    """Находит (draft_uuid, row) черновика, у которого ActiveUUID указывает на
    данную активную запись (edit-драфт) — не более одного на активную запись
    (BOPF-инвариант). Линейный скан приемлем для объёма мока."""
    for draft_uuid, row in draft_store["CheckRoots"].items():
        if row.get("ActiveUUID") == active_uuid:
            return draft_uuid, row
    return None, None


def _checks_of(root_id, draft_uuid=config.ZERO_GUID):
    """[Fix] draft_uuid selects which instance's rows to return - ZERO_GUID
    (the default, preserving every existing active-only caller) for the
    active tree, or a specific draft's own uuid for that draft's tree (see
    Common.DraftNode / _clone_children_into_draft)."""
    return [
        r for (rid, _iid), r in store["CheckItems"].items()
        if rid == root_id and r.get("DraftUUID", config.ZERO_GUID) == draft_uuid
    ]


def _barriers_of(root_id, draft_uuid=config.ZERO_GUID):
    """[Fix] See _checks_of's draft_uuid docstring - same instance selection."""
    return [
        r for (rid, _bid), r in store["Barriers"].items()
        if rid == root_id and r.get("DraftUUID", config.ZERO_GUID) == draft_uuid
    ]
