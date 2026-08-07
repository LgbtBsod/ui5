"""URL/method routing: parses the request path into (entity set, key) or a
FunctionImport call, and dispatches to query.py/draft_service.py/crud.py.
This is the one module that knows about every route shape - everything
downstream of it is oblivious to HTTP.

Refactored with type hints (PEP 484), dataclasses for configuration, and
improved separation of concerns following clean code principles.

Note: The main dispatch logic has been moved to handlers.py which implements
the Command pattern for better separation of concerns and testability.
"""
from __future__ import annotations

import re
from typing import Optional, Tuple, Match

from .handlers import RequestDispatcher

# Compiled regex patterns (kept here for use by other modules)
SINGLE_KEY_RE = re.compile(r"^(\w+)\('([^']*)'\)$")
COMPOSITE_KEY_RE = re.compile(r"^(\w+)\(RootId='([^']*)',(ItemId|BarrierId)='([^']*)'\)$")
CHECKROOT_KEY_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)$")
NAV_COLLECTION_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/(to_Basic|to_Checks|to_Barriers)$")
NAV_CREATE_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/(to_Checks|to_Barriers)$")
NAV_DRAFT_ADMIN_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/DraftAdministrativeData$")
NAV_SIBLING_RE = re.compile(r"^CheckRoots\(ActiveUUID=guid'([^']*)',DraftUUID=guid'([^']*)'\)/SiblingEntity$")


def _nav_root_id(m: Match[str]) -> str:
    """Resolves the effective RootId from a NAV_COLLECTION_RE/NAV_CREATE_RE
    match's (ActiveUUID, DraftUUID) pair — the active row's own id when
    addressed via ActiveUUID (DraftUUID = ZERO_GUID), otherwise the owning
    draft row's RootId (create-drafts carry their own uuid as RootId).
    
    Args:
        m: Regex match object from navigation pattern
        
    Returns:
        Effective RootId string
    """
    from . import config
    from . import state
    
    active_uuid, draft_uuid = m.group(1), m.group(2)
    if draft_uuid == config.ZERO_GUID:
        return active_uuid
    draft_row = state.draft_store["CheckRoots"].get(draft_uuid)
    return draft_row["RootId"] if draft_row else active_uuid


def _parse_key(url_path: str) -> Tuple[Optional[str], Optional[dict]]:
    """Returns (set_name, key_parts_dict) or (None, None).
    
    Args:
        url_path: URL path to parse
        
    Returns:
        Tuple of (entity set name, key parts dict) or (None, None)
    """
    from . import config
    import serve_config
    
    m = CHECKROOT_KEY_RE.match(url_path)
    if m:
        return "CheckRoots", {"ActiveUUID": m.group(1), "DraftUUID": m.group(2)}
    m = COMPOSITE_KEY_RE.match(url_path)
    if m:
        from urllib.parse import unquote
        set_name, root_id, id_prop, id_val = m.groups()
        return set_name, {"RootId": unquote(root_id), id_prop: unquote(id_val)}
    m = SINGLE_KEY_RE.match(url_path)
    if m:
        from urllib.parse import unquote
        set_name, key = m.groups()
        props = config.KEY_PROP_TUPLES.get(set_name) or (serve_config.REFERENCE_KEY_PROPS.get(set_name),)
        if props[0] is None:
            return None, None
        return set_name, {props[0]: unquote(key)}
    return None, None


# Singleton dispatcher instance
_dispatcher = RequestDispatcher()


def dispatch_request(
    method: str,
    rel_url: str,
    body: Optional[dict] = None,
    headers: Optional[dict] = None
) -> Tuple[int, any, str]:
    """Dispatch HTTP request to appropriate handler.
    
    Args:
        method: HTTP method (GET, POST, PUT, MERGE, PATCH, DELETE)
        rel_url: Relative URL path with query string
        body: Request body dict (for POST/PUT/PATCH)
        headers: Request headers dict
    
    Returns:
        Tuple of (http_status, response_body, content_type)
    """
    return _dispatcher.dispatch(method, rel_url, body, headers)
