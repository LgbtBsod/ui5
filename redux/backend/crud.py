"""Primitive entity-creation helpers shared by dispatch.py's POST-create and
nav-create handlers. Depends on config.py, state.py, odata_format.py only.
"""
from __future__ import annotations

from typing import Any, Dict

from . import config
from . import state
from . import odata_format


def _create_check_basic(root_id: str, body: Dict[str, Any], now: str) -> None:
    """Create CheckBasics entity.
    
    Args:
        root_id: Root entity identifier
        body: Entity data dict
        now: ISO timestamp
    """
    basic = dict(body or {})
    basic.pop("__metadata", None)
    basic["RootId"] = root_id
    basic["LastChangedAt"] = now
    state.store["CheckBasics"][root_id] = basic


def _create_check_item(
    root_id: str,
    body: Dict[str, Any],
    now: str,
    draft_uuid: str = config.ZERO_GUID
) -> str:
    """Create CheckItems entity.
    
    Args:
        root_id: Parent root identifier
        body: Entity data dict
        now: ISO timestamp
        draft_uuid: Draft instance tag (ZERO_GUID for active, draft UUID for drafts)
    
    Returns:
        Generated item ID
    
    Note:
        draft_uuid tags which "instance" this row belongs to - ZERO_GUID
        for an active-owned row, or a specific draft's own uuid for a row that
        only exists inside that draft's edit session (see Common.DraftNode /
        draft_service._clone_children_into_draft). A create-draft's own children
        are tagged with that draft's uuid too (root_id itself, by the
        create-draft convention), never ZERO_GUID, since there is no active twin
        yet.
    """
    body = dict(body or {})
    body.pop("__metadata", None)
    item_id = odata_format.generate_uuid()
    body["RootId"] = root_id
    body["ItemId"] = item_id
    body["DraftUUID"] = draft_uuid
    body["LastChangedAt"] = now
    state.store["CheckItems"][(root_id, item_id)] = body
    return item_id


def _create_barrier(
    root_id: str,
    body: Dict[str, Any],
    now: str,
    draft_uuid: str = config.ZERO_GUID
) -> str:
    """Create Barriers entity.
    
    Args:
        root_id: Parent root identifier
        body: Entity data dict
        now: ISO timestamp
        draft_uuid: Draft instance tag (ZERO_GUID for active, draft UUID for drafts)
    
    Returns:
        Generated barrier ID
    
    Note:
        See _create_check_item's draft_uuid docstring - same tagging.
    """
    body = dict(body or {})
    body.pop("__metadata", None)
    barrier_id = odata_format.generate_uuid()
    body["RootId"] = root_id
    body["BarrierId"] = barrier_id
    body["DraftUUID"] = draft_uuid
    body["LastChangedAt"] = now
    state.store["Barriers"][(root_id, barrier_id)] = body
    return barrier_id
