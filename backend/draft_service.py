"""Draft-lifecycle business logic: CheckRootPreparationAction/
ActivationAction/DiscardAction and every store-mutating helper they rely on
(children snapshot/restore/purge/clone/re-tag, required-field validation).

[Fix, toolkit pass] The actual state machine now lives in
toolkit/draft.py's DraftEngine - this module is a thin, config-only wrapper
around ONE module-level engine instance, built from THIS project's own
CheckRoots/CheckItems/Barriers/CheckBasics shape. Every function below
keeps its EXACT original name and signature (backend/__init__.py
re-exports all of them by name, and tests/test_serve.py's `serve.XXX`
attribute access depends on that staying true) - only the body changed, to
a one-line delegation into the engine. See
C:\\Users\\lgbtb\\.claude\\plans\\rosy-swinging-nest.md for the full design
rationale and the adversarial review that shaped this split.

Depends on config.py, state.py, odata_format.py, resolvers.py (for
wrap_entity - draft_service mutates state and hands back a wrapped view of
the result; resolvers itself never imports this module, breaking what would
otherwise be an import cycle between "mutate a draft" and "read a draft's
admin data"). [Fix] DraftEngine itself (toolkit/draft.py) works ONLY with
raw store rows and never calls resolvers - wrap_entity is applied HERE,
after the engine returns, never inside the engine (see _wrap_if_row below).
"""
from __future__ import annotations

from typing import Any, Dict, List, Optional, Tuple

from . import config
from . import state
from . import resolvers
from toolkit.draft import ChildEntityConfig, DraftRootConfig, DraftEngine

_ROOT_CONFIG = DraftRootConfig(
    entity_set="CheckRoots",
    # [Fix] Explicitly "RootId" - the store's OWN indexing key. NOT derived
    # from config.KEY_PROP_TUPLES["CheckRoots"] (= ("ActiveUUID", "DraftUUID"),
    # the OData URL key-segment shape, unrelated to how state.store is
    # actually indexed) - see DraftRootConfig's own docstring for why
    # conflating the two would silently break every store lookup.
    root_key_prop="RootId",
    children=(
        ChildEntityConfig(entity_set="CheckItems", id_prop="ItemId", nav_property="to_Checks"),
        ChildEntityConfig(entity_set="Barriers", id_prop="BarrierId", nav_property="to_Barriers"),
    ),
    basic_proxy_entity_set="CheckBasics",
    required_root_fields=config.REQUIRED_ROOT_FIELDS,
    required_field_alternates=config.REQUIRED_FIELD_ALTERNATES,
    required_child_field=config.REQUIRED_CHILD_FIELD,
    required_child_field_label=config.REQUIRED_CHILD_FIELD_LABEL,
    default_owner=config.MOCK_USER,
)
_engine = DraftEngine(_ROOT_CONFIG, state.store, state.draft_store)


def _wrap_if_row(result):
    """[Fix] resolvers.wrap_entity stays OUTSIDE toolkit/draft.py - it's
    entirely domain-specific (KPIs/criticality/basic-proxy-merge via
    compute_check_root_view). The engine returns raw store rows and
    sentinels; this applies wrap_entity to the raw-row case only, leaving
    every other sentinel shape (None / ("foreign_lock", ...) /
    ("validation_error", ...) / "discarded-create-only") passed through
    unchanged, exactly as the original functions returned them."""
    if isinstance(result, dict):
        return resolvers.wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], result, None)
    return result


def _missing_required_fields(merged_view: Dict[str, Any]) -> List[Tuple[str, str]]:
    """Find missing required fields in merged view - see
    toolkit.draft.DraftEngine.missing_required_fields."""
    return _engine.missing_required_fields(merged_view)


def _missing_required_child_fields(root_id: str, instance_tag: str) -> List[Dict[str, str]]:
    """Find missing required child fields - see
    toolkit.draft.DraftEngine.missing_required_child_fields."""
    return _engine.missing_required_child_fields(root_id, instance_tag)


def _snapshot_children(root_id):
    """CheckBasics-only root-scoped proxy snapshot - see
    toolkit.draft.DraftEngine.snapshot_children."""
    return _engine.snapshot_children(root_id)


def _restore_children(root_id, snapshot):
    """Inverse of _snapshot_children - see
    toolkit.draft.DraftEngine.restore_children."""
    _engine.restore_children(root_id, snapshot)


def _purge_children(root_id):
    """Removes every CheckBasics/CheckItems/Barriers row for root_id - see
    toolkit.draft.DraftEngine.purge_children."""
    _engine.purge_children(root_id)


def _clone_children_into_draft(root_id, draft_uuid):
    """Clones active-tagged CheckItems/Barriers rows into draft-tagged
    copies - see toolkit.draft.DraftEngine.clone_children_into_draft."""
    _engine.clone_children_into_draft(root_id, draft_uuid)


def _purge_tagged_children(root_id, draft_uuid):
    """Removes only this draft's own tagged child rows - see
    toolkit.draft.DraftEngine.purge_tagged_children."""
    _engine.purge_tagged_children(root_id, draft_uuid)


def _activate_tagged_children(root_id, draft_uuid):
    """Re-tags this draft's child rows to become the new active children -
    see toolkit.draft.DraftEngine.activate_tagged_children."""
    _engine.activate_tagged_children(root_id, draft_uuid)


def _draft_owner_mismatch(draft_uuid, requesting_user):
    """CheckRootPreparationAction-adjacent foreign-lock guard for every
    write after the initial Prepare - see
    toolkit.draft.DraftEngine.owner_mismatch."""
    return _engine.owner_mismatch(draft_uuid, requesting_user)


def _draft_prepare(active_uuid, preserve_changes=True, requesting_user=config.MOCK_USER, draft_uuid_hint=None):
    """[Фаза 5] CheckRootPreparationAction — готовит edit-черновик для
    активной записи active_uuid; повторный вызов идемпотентен. None, если
    активной записи с таким ActiveUUID нет вообще. See
    toolkit.draft.DraftEngine.prepare for the full state-machine logic."""
    return _wrap_if_row(_engine.prepare(
        active_uuid,
        preserve_changes=preserve_changes,
        requesting_user=requesting_user,
        draft_uuid_hint=draft_uuid_hint,
    ))


def _draft_activate(draft_uuid):
    """[Фаза 5] CheckRootActivationAction — переносит черновик draft_uuid в
    активную запись и удаляет черновик. None, если DraftUUID не найден.
    Returns ("validation_error", missing, missing_children) instead of
    activating when a required field is blank. See
    toolkit.draft.DraftEngine.activate."""
    return _wrap_if_row(_engine.activate(draft_uuid))


def _draft_discard(draft_uuid):
    """[Фаза 5] CheckRootDiscardAction — удаляет черновик draft_uuid без
    применения к активной записи. None, если DraftUUID не найден. See
    toolkit.draft.DraftEngine.discard."""
    return _wrap_if_row(_engine.discard(draft_uuid))
