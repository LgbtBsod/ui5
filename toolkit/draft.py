"""Generic BOPF/Fiori-Elements draft-protocol state machine - Prepare/
Activate/Discard plus composition-child clone/re-tag/purge - config-driven
via DraftRootConfig/ChildEntityConfig, with zero knowledge of any concrete
entity model.

Generalized out of backend/draft_service.py (the ONLY place this pattern
existed before this toolkit). backend/draft_service.py now instantiates one
DraftEngine for CheckRoots and delegates every existing function to it - see
that module for the "thin wrapper" side of this split.

Scope boundary (deliberate, confirmed by adversarial design review before
implementation - see the plan file referenced in draft_service.py):
  - DraftEngine works ONLY with raw store rows. It never calls into any
    resolver/view-computation layer (KPIs, criticality, display-name
    lookups, etc.) - callers that need a resolved/wrapped response apply
    that themselves on top of this engine's raw dict results. This mirrors
    the original code exactly: the real required-field gate on Activate
    always validated raw stored fields, never a resolved view.
  - "Composition children" here means real Common.DraftNode entities -
    tagged per-row with a DraftUUID (ZERO_GUID = belongs to the active
    tree, a specific draft's own uuid = belongs to that draft's edit
    session), keyed by a composite (root_id, child_id) tuple in `store`.
    A root MAY additionally have exactly one "basic proxy" entity set
    (root-scoped, snapshot/restore-based rollback instead of tag-based -
    see backend/draft_service.py's snapshot_children docstring for why
    CheckBasics needs this instead of being a draft node itself). These
    are two orthogonal, independently-optional axes - basic_proxy_entity_set
    is NOT one of `children`.
"""
from __future__ import annotations

import uuid as _uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple, Union

#: BOPF sentinel for "no draft"/"no active counterpart" - the real SAP
#: convention (all-zero GUID), not project-specific. Must match whatever
#: literal value the store's own rows use for the same purpose (see
#: backend/config.py's ZERO_GUID, which is this exact string).
ZERO_GUID = "00000000-0000-0000-0000-000000000000"


def _generate_uuid() -> str:
    return str(_uuid.uuid4())


def _odata_date() -> str:
    return "/Date(%d)/" % int(datetime.now(timezone.utc).timestamp() * 1000)


@dataclass(frozen=True)
class ChildEntityConfig:
    """One composition-child (real Common.DraftNode) entity set.

    entity_set:   store dict key, e.g. "CheckItems" - store[entity_set] is a
                  dict[(root_id, child_id), row].
    id_prop:      the child's own id property name, e.g. "ItemId".
    nav_property: the root's navigation property to this child, e.g.
                  "to_Checks" - not used internally by DraftEngine, carried
                  through into missing_required_child_fields()'s "nav" key
                  for callers building NAV-qualified validation targets.
    """
    entity_set: str
    id_prop: str
    nav_property: str


@dataclass(frozen=True)
class DraftRootConfig:
    """Everything DraftEngine needs to know about one draft-enabled root
    entity set.

    entity_set:        store dict key, e.g. "CheckRoots".
    root_key_prop:      the store's OWN indexing key for this root - e.g.
                        "RootId". MUST be the field the store dict is
                        actually keyed by, NOT an OData URL key-segment
                        shape (a service's ActiveUUID/DraftUUID pair is a
                        URL/response concern, unrelated to how the store
                        dict itself is indexed - conflating the two silently
                        breaks every store lookup).
    children:           composition-child configs (see ChildEntityConfig) -
                        () if this root has none.
    basic_proxy_entity_set: name of an optional ADDITIONAL root-scoped
                        entity set that is NOT a draft node but still needs
                        snapshot/restore on Discard (edits write straight
                        into the active table during an edit session) - None
                        if this root has no such proxy table.
    required_root_fields:    {field_name: label} - root fields that must be
                        non-empty before Activate succeeds.
    required_field_alternates: {field_name: alternate_field_name} - a
                        required field is satisfied if EITHER it or its
                        alternate is non-empty.
    required_child_field:      property name every child row must have
                        non-None before Activate succeeds, or None to skip
                        child validation entirely.
    required_child_field_label: display label for required_child_field.
    default_owner:      fallback "owner" identity used only defensively
                        (a draft row missing InProcessByUser, which normal
                        Prepare always sets) - matches the mock's own
                        "current user" placeholder concept.
    """
    entity_set: str
    root_key_prop: str
    children: Tuple[ChildEntityConfig, ...] = ()
    basic_proxy_entity_set: Optional[str] = None
    required_root_fields: Dict[str, str] = field(default_factory=dict)
    required_field_alternates: Dict[str, str] = field(default_factory=dict)
    required_child_field: Optional[str] = None
    required_child_field_label: Optional[str] = None
    default_owner: str = "SYSTEM"


#: Sentinel return shapes, preserved EXACTLY from the original
#: backend/draft_service.py so callers (backend/handlers.py) that
#: pattern-match on them need zero changes:
#:   None                                      - not found
#:   dict                                      - raw row, success
#:   ("foreign_lock", draft_row)                - owned by another user
#:   ("validation_error", missing, missing_children) - activate only, 3-tuple
#:   "discarded-create-only"                    - bare string, NOT a tuple
DraftResult = Union[None, Dict[str, Any], Tuple[str, Any], Tuple[str, Any, Any], str]


class DraftEngine:
    """No state of its own beyond the config/store references passed to the
    constructor - store/draft_store are the caller's own module-level dicts
    (never reassigned, only mutated - safe to hold by reference), so it's
    safe to instantiate exactly one DraftEngine per draft-enabled service at
    import time."""

    def __init__(self, config: DraftRootConfig, store: Dict[str, dict], draft_store: Dict[str, dict]):
        self.config = config
        self.store = store
        self.draft_store = draft_store

    # -- root store access -------------------------------------------------

    @property
    def _root_set(self) -> dict:
        return self.store[self.config.entity_set]

    @property
    def _draft_root_set(self) -> dict:
        return self.draft_store[self.config.entity_set]

    def _find_draft_by_active_uuid(self, active_uuid: str) -> Tuple[Optional[str], Optional[dict]]:
        for draft_uuid, row in self._draft_root_set.items():
            if row.get("ActiveUUID") == active_uuid:
                return draft_uuid, row
        return None, None

    # -- public state machine -----------------------------------------------

    def owner_mismatch(self, draft_uuid: str, requesting_user: str) -> Optional[dict]:
        """Returns the draft row if it exists and is owned by a DIFFERENT
        user than requesting_user (caller should reject the write), else
        None (own draft, or draft_uuid doesn't exist - caller's normal
        not-found handling covers that case)."""
        draft_row = self._draft_root_set.get(draft_uuid)
        if draft_row is not None and draft_row.get("InProcessByUser", self.config.default_owner) != requesting_user:
            return draft_row
        return None

    def prepare(
        self,
        active_uuid: str,
        preserve_changes: bool = True,
        requesting_user: Optional[str] = None,
        draft_uuid_hint: Optional[str] = None,
    ) -> DraftResult:
        requesting_user = requesting_user if requesting_user is not None else self.config.default_owner
        if active_uuid == ZERO_GUID:
            # In-progress create-draft (own ActiveUUID is still ZERO_GUID) -
            # resolve straight from the caller-supplied draft_uuid_hint
            # instead of treating ZERO_GUID as "no such active row".
            draft_row = self._draft_root_set.get(draft_uuid_hint)
            if draft_row is None or draft_row.get("ActiveUUID", ZERO_GUID) != ZERO_GUID:
                return None
            if self.owner_mismatch(draft_uuid_hint, requesting_user) is not None:
                return ("foreign_lock", draft_row)
            return draft_row
        if active_uuid not in self._root_set:
            return None
        draft_uuid, draft_row = self._find_draft_by_active_uuid(active_uuid)
        if draft_row is not None and preserve_changes and draft_row.get("InProcessByUser", self.config.default_owner) != requesting_user:
            return ("foreign_lock", draft_row)
        if draft_row is not None and not preserve_changes:
            self._draft_root_set.pop(draft_uuid, None)
            self.restore_children(active_uuid, draft_row.get("_child_snapshot"))
            self.purge_tagged_children(active_uuid, draft_uuid)
            draft_row = None
        if draft_row is None:
            draft_uuid = _generate_uuid()
            draft_row = dict(self._root_set[active_uuid])
            draft_row["ActiveUUID"] = active_uuid
            draft_row["DraftUUID"] = draft_uuid
            draft_row["IsActiveEntity"] = False
            draft_row["InProcessByUser"] = requesting_user
            draft_row["CreatedByUser"] = requesting_user
            draft_row["_child_snapshot"] = self.snapshot_children(active_uuid)
            draft_row["_draft_created_at"] = _odata_date()
            self._draft_root_set[draft_uuid] = draft_row
            self.clone_children_into_draft(active_uuid, draft_uuid)
        return draft_row

    def activate(self, draft_uuid: str) -> DraftResult:
        draft_row = self._draft_root_set.get(draft_uuid)
        if draft_row is None:
            return None
        root_id = draft_row[self.config.root_key_prop]
        raw_basic: dict = {}
        if self.config.basic_proxy_entity_set:
            raw_basic = self.store.get(self.config.basic_proxy_entity_set, {}).get(root_id) or {}
        missing = self.missing_required_fields({**draft_row, **raw_basic})
        missing_children = self.missing_required_child_fields(root_id, draft_uuid)
        if missing or missing_children:
            return ("validation_error", missing, missing_children)
        self._draft_root_set.pop(draft_uuid, None)
        draft_row.pop("_child_snapshot", None)
        self.activate_tagged_children(root_id, draft_uuid)
        exclude = {self.config.root_key_prop, "ActiveUUID", "DraftUUID", "IsActiveEntity"}
        root_set = self._root_set
        if root_id in root_set:
            active_row = root_set[root_id]
            for k, v in draft_row.items():
                if k not in exclude:
                    active_row[k] = v
            active_row["LastChangedAt"] = _odata_date()
        else:
            draft_row["LastChangedAt"] = _odata_date()
            draft_row["ActiveUUID"] = root_id
            draft_row["DraftUUID"] = ZERO_GUID
            draft_row["IsActiveEntity"] = True
            root_set[root_id] = draft_row
        return root_set[root_id]

    def discard(self, draft_uuid: str) -> DraftResult:
        draft_row = self._draft_root_set.pop(draft_uuid, None)
        if draft_row is None:
            return None
        root_id = draft_row[self.config.root_key_prop]
        root_set = self._root_set
        if root_id not in root_set:
            self.purge_children(root_id)
            return "discarded-create-only"
        self.restore_children(root_id, draft_row.get("_child_snapshot"))
        self.purge_tagged_children(root_id, draft_uuid)
        return root_set[root_id]

    # -- required-field validation ------------------------------------------

    def missing_required_fields(self, merged_view: Dict[str, Any]) -> List[Tuple[str, str]]:
        missing = []
        for f, label in self.config.required_root_fields.items():
            val = merged_view.get(f)
            if val is not None and val != "":
                continue
            alt_field = self.config.required_field_alternates.get(f)
            alt_val = merged_view.get(alt_field) if alt_field else None
            if alt_val is None or alt_val == "":
                missing.append((f, label))
        return missing

    def missing_required_child_fields(self, root_id: str, instance_tag: str) -> List[Dict[str, str]]:
        missing = []
        if not self.config.required_child_field:
            return missing
        for child in self.config.children:
            child_set = self.store.get(child.entity_set, {})
            for (rid, item_id), row in child_set.items():
                if (
                    rid == root_id
                    and row.get("DraftUUID", ZERO_GUID) == instance_tag
                    and row.get(self.config.required_child_field) is None
                ):
                    missing.append({
                        "nav": child.nav_property,
                        "id_prop": child.id_prop,
                        "item_id": item_id,
                        "field": self.config.required_child_field,
                        "label": self.config.required_child_field_label,
                    })
        return missing

    # -- basic-proxy snapshot/restore (root-scoped, non-draft-node table) --

    def snapshot_children(self, root_id: str) -> Optional[Dict[str, Optional[dict]]]:
        if not self.config.basic_proxy_entity_set:
            return None
        proxy_set = self.store[self.config.basic_proxy_entity_set]
        return {self.config.basic_proxy_entity_set: dict(proxy_set[root_id]) if root_id in proxy_set else None}

    def restore_children(self, root_id: str, snapshot: Optional[Dict[str, Optional[dict]]]) -> None:
        if snapshot is None or not self.config.basic_proxy_entity_set:
            return
        proxy_set = self.store[self.config.basic_proxy_entity_set]
        proxy_row = snapshot.get(self.config.basic_proxy_entity_set)
        if proxy_row is not None:
            proxy_set[root_id] = proxy_row
        else:
            proxy_set.pop(root_id, None)

    def purge_children(self, root_id: str) -> None:
        """Removes every basic-proxy AND composition-child row for root_id
        regardless of instance tag - used when an abandoned create-draft
        (never activated) is discarded, so rows created during that session
        don't leak forever as orphaned, still-queryable-by-key records."""
        if self.config.basic_proxy_entity_set:
            self.store[self.config.basic_proxy_entity_set].pop(root_id, None)
        for child in self.config.children:
            child_set = self.store.get(child.entity_set, {})
            for key in [k for k in child_set if k[0] == root_id]:
                del child_set[key]

    # -- composition-child (Common.DraftNode) clone/purge/re-tag -----------

    def clone_children_into_draft(self, root_id: str, draft_uuid: str) -> None:
        """Deep-clones the active-tagged (ZERO_GUID) child rows into fresh
        draft-tagged copies (new ids, DraftUUID=draft_uuid) - active rows
        stay untouched for the whole edit session, so Discard is just
        dropping the draft-tagged copies and Activate is a cheap re-tag
        (no field-by-field merge needed for children)."""
        for child in self.config.children:
            child_set = self.store.setdefault(child.entity_set, {})
            for (rid, _cid), row in list(child_set.items()):
                if rid == root_id and row.get("DraftUUID", ZERO_GUID) == ZERO_GUID:
                    clone = dict(row)
                    clone[child.id_prop] = _generate_uuid()
                    clone["DraftUUID"] = draft_uuid
                    child_set[(root_id, clone[child.id_prop])] = clone

    def purge_tagged_children(self, root_id: str, draft_uuid: str) -> None:
        """Removes only the child rows tagged with this specific draft_uuid
        for root_id - used by Discard (drop the draft's own node copies,
        active untouched) and orphaned-draft cleanup."""
        for child in self.config.children:
            child_set = self.store.get(child.entity_set, {})
            for key in [
                k for k, v in child_set.items()
                if k[0] == root_id and v.get("DraftUUID", ZERO_GUID) == draft_uuid
            ]:
                del child_set[key]

    def activate_tagged_children(self, root_id: str, draft_uuid: str) -> None:
        """The other half of clone_children_into_draft, called on
        successful Activation: deletes the current active-tagged rows for
        root_id, then re-tags every row belonging to draft_uuid to
        ZERO_GUID so they become the new active children."""
        for child in self.config.children:
            child_set = self.store.get(child.entity_set, {})
            for key in [
                k for k, v in child_set.items()
                if k[0] == root_id and v.get("DraftUUID", ZERO_GUID) == ZERO_GUID
            ]:
                del child_set[key]
            for row in child_set.values():
                if row.get(self.config.root_key_prop) == root_id and row.get("DraftUUID", ZERO_GUID) == draft_uuid:
                    row["DraftUUID"] = ZERO_GUID
