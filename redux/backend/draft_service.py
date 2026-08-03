"""Draft-lifecycle business logic: CheckRootPreparationAction/
ActivationAction/DiscardAction and every store-mutating helper they rely on
(children snapshot/restore/purge/clone/re-tag, required-field validation).
Depends on config.py, state.py, odata_format.py, resolvers.py (for
wrap_entity - draft_service mutates state and hands back a wrapped view of
the result; resolvers itself never imports this module, breaking what would
otherwise be an import cycle between "mutate a draft" and "read a draft's
admin data").
"""
from . import config
from . import state
from . import odata_format
from . import resolvers


def _missing_required_fields(merged_view):
    """merged_view: any dict exposing the required fields directly (either
    the merged compute_check_root_view() output, or a root_fields/to_basic
    pair pre-merged by the caller). Returns a list of (field_name, label)
    tuples for every missing field, empty if nothing's missing.

    [Fix] Used to return just the label strings - fine for a single combined
    MessageBox text, but not enough to build per-field `target`-bound
    messages (see dispatch.py's VALIDATION_ERROR response, which needs the
    actual property NAME to underline the right SmartField red)."""
    missing = []
    for field, label in config.REQUIRED_ROOT_FIELDS.items():
        val = merged_view.get(field)
        if val is not None and val != "":
            continue
        alt_field = config.REQUIRED_FIELD_ALTERNATES.get(field)
        alt_val = merged_view.get(alt_field) if alt_field else None
        if alt_val is None or alt_val == "":
            missing.append((field, label))
    return missing


def _missing_required_child_fields(root_id, instance_tag):
    """[Fix, exhaustive-sweep pass] Returns a list of {"nav", "id_prop",
    "item_id", "field", "label"} dicts, one per tagged CheckItem/Barrier row
    (instance_tag = the activating draft's own DraftUUID, or ZERO_GUID for
    the active tree) whose Result is still None (never assessed). Separate
    from _missing_required_fields (root-only, bare-field targets) because
    each entry here needs enough to build a NAV-QUALIFIED errordetails[]
    target (see dispatch.py's VALIDATION_ERROR branch) - a bare "Result"
    target would ambiguously point at every child row at once instead of
    the one that's actually incomplete."""
    missing = []
    for (rid, item_id), row in state.store["CheckItems"].items():
        if rid == root_id and row.get("DraftUUID", config.ZERO_GUID) == instance_tag and row.get(config.REQUIRED_CHILD_FIELD) is None:
            missing.append({"nav": "to_Checks", "id_prop": "ItemId", "item_id": item_id, "field": config.REQUIRED_CHILD_FIELD, "label": config.REQUIRED_CHILD_FIELD_LABEL})
    for (rid, barrier_id), row in state.store["Barriers"].items():
        if rid == root_id and row.get("DraftUUID", config.ZERO_GUID) == instance_tag and row.get(config.REQUIRED_CHILD_FIELD) is None:
            missing.append({"nav": "to_Barriers", "id_prop": "BarrierId", "item_id": barrier_id, "field": config.REQUIRED_CHILD_FIELD, "label": config.REQUIRED_CHILD_FIELD_LABEL})
    return missing


def _snapshot_children(root_id):
    """[Fix] CheckBasics stays root-scoped (proxy-fields form, not a real
    draft node) - edits made against it while a parent edit-draft is open
    still write straight into the active table (see dispatch.py's
    NAV_CREATE_RE handling), so it still needs snapshot/restore for Discard
    to undo those edits. CheckItems/Barriers no longer need this: they're
    real draft nodes now (Common.DraftNode, see _clone_children_into_draft/
    _purge_tagged_children) - each row is tagged with which instance it
    belongs to, so discarding a draft is just deleting its own tagged rows,
    never touching the active ones in the first place. Deep-copies so later
    in-place mutation of the live row can't corrupt the snapshot."""
    return {
        "CheckBasics": dict(state.store["CheckBasics"][root_id]) if root_id in state.store["CheckBasics"] else None,
    }


def _restore_children(root_id, snapshot):
    """Inverse of _snapshot_children (CheckBasics only - see that docstring)."""
    if snapshot is None:
        return
    if snapshot["CheckBasics"] is not None:
        state.store["CheckBasics"][root_id] = snapshot["CheckBasics"]
    else:
        state.store["CheckBasics"].pop(root_id, None)


def _purge_children(root_id):
    """[Fix] Removes every CheckBasics/CheckItems/Barriers row for root_id
    regardless of instance tag - used when a create-draft (never activated)
    is discarded/its parent deleted, so composition rows created during that
    abandoned session don't leak forever as orphaned, still-queryable-by-key
    records. Safe to ignore tags here: a create-draft's root_id never had an
    active existence, so every child row for it - whatever its DraftUUID tag
    - belongs to this one abandoned session."""
    state.store["CheckBasics"].pop(root_id, None)
    for key in [k for k in state.store["CheckItems"] if k[0] == root_id]:
        del state.store["CheckItems"][key]
    for key in [k for k in state.store["Barriers"] if k[0] == root_id]:
        del state.store["Barriers"][key]


def _clone_children_into_draft(root_id, draft_uuid):
    """[Fix] CheckItems/Barriers are real draft nodes (Common.DraftNode) -
    Prepare deep-clones the active-tagged rows into fresh draft-tagged
    copies (new ItemIds/BarrierIds, DraftUUID=draft_uuid), leaving the
    active rows completely untouched for the whole edit session. This is
    what lets Discard simply drop the draft-tagged copies (see
    _purge_tagged_children) with zero rollback bookkeeping, and lets
    Activate become a cheap re-tag (see _draft_activate) instead of a
    field-by-field merge."""
    for (rid, _item_id), row in list(state.store["CheckItems"].items()):
        if rid == root_id and row.get("DraftUUID", config.ZERO_GUID) == config.ZERO_GUID:
            clone = dict(row)
            clone["ItemId"] = odata_format.generate_uuid()
            clone["DraftUUID"] = draft_uuid
            state.store["CheckItems"][(root_id, clone["ItemId"])] = clone
    for (rid, _barrier_id), row in list(state.store["Barriers"].items()):
        if rid == root_id and row.get("DraftUUID", config.ZERO_GUID) == config.ZERO_GUID:
            clone = dict(row)
            clone["BarrierId"] = odata_format.generate_uuid()
            clone["DraftUUID"] = draft_uuid
            state.store["Barriers"][(root_id, clone["BarrierId"])] = clone


def _purge_tagged_children(root_id, draft_uuid):
    """[Fix] Removes only the CheckItems/Barriers rows tagged with this
    specific draft_uuid for root_id - used by Discard (drop the draft's own
    node copies, active untouched) and by cleanup of an orphaned draft's
    nodes (see dispatch.py's active CheckRoots DELETE handler)."""
    for key in [
        k for k, v in state.store["CheckItems"].items()
        if k[0] == root_id and v.get("DraftUUID", config.ZERO_GUID) == draft_uuid
    ]:
        del state.store["CheckItems"][key]
    for key in [
        k for k, v in state.store["Barriers"].items()
        if k[0] == root_id and v.get("DraftUUID", config.ZERO_GUID) == draft_uuid
    ]:
        del state.store["Barriers"][key]


def _activate_tagged_children(root_id, draft_uuid):
    """[Fix] The other half of _clone_children_into_draft - called on
    successful Activation. Deletes the current active-tagged (ZERO_GUID)
    rows for root_id, then re-tags every row belonging to draft_uuid to
    ZERO_GUID so they become the new active children. Cheap by design: no
    field-by-field copy needed, just flip which "instance" each row counts
    as."""
    for key in [
        k for k, v in state.store["CheckItems"].items()
        if k[0] == root_id and v.get("DraftUUID", config.ZERO_GUID) == config.ZERO_GUID
    ]:
        del state.store["CheckItems"][key]
    for key in [
        k for k, v in state.store["Barriers"].items()
        if k[0] == root_id and v.get("DraftUUID", config.ZERO_GUID) == config.ZERO_GUID
    ]:
        del state.store["Barriers"][key]
    for row in state.store["CheckItems"].values():
        if row.get("RootId") == root_id and row.get("DraftUUID", config.ZERO_GUID) == draft_uuid:
            row["DraftUUID"] = config.ZERO_GUID
    for row in state.store["Barriers"].values():
        if row.get("RootId") == root_id and row.get("DraftUUID", config.ZERO_GUID) == draft_uuid:
            row["DraftUUID"] = config.ZERO_GUID


def _draft_owner_mismatch(draft_uuid, requesting_user):
    """[Fix, exhaustive-sweep pass] sap.ui.generic.app.transaction.
    TransactionController.prototype.editEntity (real 1.71.84 source) only
    invokes the foreign-lock-checking EditAction/CheckRootPreparationAction
    path when the addressed context's own IsActiveEntity is true - once a
    context IS already a draft, the framework never re-enters that gate for
    later writes to it (PATCH/MERGE/DELETE, Activate, Discard addressed
    directly by the draft's own key). Without this check, any mock user who
    knows/guesses another user's DraftUUID (e.g. via /dev/locks, or a stale
    deep-link) could Activate, Discard, or edit that user's in-progress
    draft outright - _draft_prepare's own foreign-lock branch only guards
    the ONE-TIME Prepare call, not everything after it.

    Returns the draft row if draft_uuid exists and is owned by a DIFFERENT
    user than requesting_user (caller should reject the write), else None
    (write is allowed - own draft, or draft_uuid doesn't exist at all, which
    the caller's normal 404 handling already covers)."""
    draft_row = state.draft_store["CheckRoots"].get(draft_uuid)
    if draft_row is not None and draft_row.get("InProcessByUser", config.MOCK_USER) != requesting_user:
        return draft_row
    return None


def _draft_prepare(active_uuid, preserve_changes=True, requesting_user=config.MOCK_USER, draft_uuid_hint=None):
    """[Фаза 5] CheckRootPreparationAction — готовит edit-черновик для
    активной записи active_uuid; повторный вызов идемпотентен (возвращает уже
    существующий черновик, не пересоздаёт его поверх правок пользователя).
    None, если активной записи с таким ActiveUUID нет вообще.

    [Fix, exhaustive-sweep pass] draft_uuid_hint - PreparationAction is
    re-invoked (not just called once at initial Edit-click) whenever
    Common.SideEffects fires with the default EffectTypes="ValueChange" (see
    the existing ChecksAndBarriersRecalc SideEffects, confirmed live to
    trigger a PreparationAction call). For an EDIT-draft that's a harmless
    no-op re-resolve of an existing active_uuid. But for a CREATE-draft
    (never activated), the context's OWN ActiveUUID field is genuinely
    config.ZERO_GUID (see dispatch.py's POST /CheckRoots handler) - live-
    verified that the real framework sends exactly
    "CheckRootPreparationAction?ActiveUUID=guid'00000000...'&DraftUUID=guid'<real>'"
    in that case (both are declared FunctionImport parameters - see
    metadata.xml's comment on why DraftUUID must be declared even though the
    initial-Edit-click path ignores it), and ZERO_GUID is never a key in
    state.store["CheckRoots"] - so without this branch, every SideEffects-
    triggered recalc on an in-progress Create (e.g. adding/deleting a
    CheckItem/Barrier row before ever hitting Save) would 404. Resolves
    straight from the caller-supplied DraftUUID instead of treating
    ZERO_GUID as "no such active row".

    [Fix] preserve_changes=False (see the metadata.xml PreserveChanges
    parameter) discards any existing in-progress edit-draft for this row and
    starts a fresh one from the current active data, instead of always
    silently resuming whatever draft already existed - the standard "keep
    your unsaved draft or discard it and start over" choice real BOPF offers
    when re-editing an object with an already-open draft.

    [Fix] Foreign-user-lock detection - matches the real convention
    sap.ui.generic.app's CRUDManager/DraftController actually implement
    (checkForForeignUserLock/fnCallEdit's 409 branch): when preserve_changes
    is true (the framework's default "strategy 2" - always try to resume/
    create, but tell me if someone ELSE already has a non-mine draft open)
    and an existing draft is owned by a DIFFERENT user than requesting_user,
    return the ("foreign_lock", draft_row) sentinel instead of silently
    handing back someone else's in-progress edits. The draft itself is left
    completely untouched. Only reachable now that requesting_user can
    actually differ per request (see state._resolve_mock_user / X-Mock-User
    header) - previously this mock had exactly one identity, so this branch
    could never fire."""
    if active_uuid == config.ZERO_GUID:
        draft_row = state.draft_store["CheckRoots"].get(draft_uuid_hint)
        if draft_row is None or draft_row.get("ActiveUUID", config.ZERO_GUID) != config.ZERO_GUID:
            return None
        if _draft_owner_mismatch(draft_uuid_hint, requesting_user) is not None:
            return ("foreign_lock", draft_row)
        return resolvers.wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], draft_row, None)
    if active_uuid not in state.store["CheckRoots"]:
        return None
    draft_uuid, draft_row = state._find_draft_by_active_uuid(active_uuid)
    if draft_row is not None and preserve_changes and draft_row.get("InProcessByUser", config.MOCK_USER) != requesting_user:
        return ("foreign_lock", draft_row)
    if draft_row is not None and not preserve_changes:
        state.draft_store["CheckRoots"].pop(draft_uuid, None)
        _restore_children(active_uuid, draft_row.get("_child_snapshot"))
        _purge_tagged_children(active_uuid, draft_uuid)
        draft_row = None
    if draft_row is None:
        draft_uuid = odata_format.generate_uuid()
        draft_row = dict(state.store["CheckRoots"][active_uuid])
        draft_row["ActiveUUID"] = active_uuid
        draft_row["DraftUUID"] = draft_uuid
        draft_row["IsActiveEntity"] = False
        draft_row["InProcessByUser"] = requesting_user
        draft_row["CreatedByUser"] = requesting_user
        draft_row["_child_snapshot"] = _snapshot_children(active_uuid)
        draft_row["_draft_created_at"] = odata_format.odata_date()
        state.draft_store["CheckRoots"][draft_uuid] = draft_row
        _clone_children_into_draft(active_uuid, draft_uuid)
    return resolvers.wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], draft_row, None)


def _draft_activate(draft_uuid):
    """[Фаза 5] CheckRootActivationAction — переносит черновик draft_uuid в
    активную запись (кроме технических ActiveUUID/DraftUUID/IsActiveEntity) и
    удаляет черновик. Create-драфт (ActiveUUID черновика ещё ZERO_GUID) сам
    становится активной записью под тем же RootId, каким был создан (см. POST
    CheckRoots) — новый id не генерируется. None, если DraftUUID не найден.

    [Fix] Returns ("validation_error", [missing_labels]) instead of
    activating when a required field is blank - previously the only
    server-side required-field gate ran on initial POST create (see
    config.REQUIRED_ROOT_FIELDS), so editing an existing object, blanking a
    required field, and clicking Save/Activate silently succeeded. The draft
    is left untouched (not popped) so the user can fix the field and retry."""
    draft_row = state.draft_store["CheckRoots"].get(draft_uuid)
    if draft_row is None:
        return None
    root_id = draft_row["RootId"]
    # [Fix] Validate against the RAW stored fields (draft row + CheckBasics),
    # matching exactly what the POST-create gate checks - NOT
    # compute_check_root_view's resolved/display view. ObserverFullname/
    # ObservedFullname in particular are display-only there (resolve_check_basic
    # always overwrites them from a Persons/Perner lookup, ignoring whatever
    # raw text was stored), so checking the resolved view would reject a
    # perfectly valid, already-created object whose free-text name doesn't
    # happen to match a Perner record.
    raw_basic = state.store["CheckBasics"].get(root_id) or {}
    missing = _missing_required_fields({**draft_row, **raw_basic})
    # [Fix, exhaustive-sweep pass] Child rows (CheckItems/Barriers) get their
    # own required-field gate here too - instance_tag is draft_uuid itself
    # (this draft's own tagged copies), NOT ZERO_GUID, mirroring
    # compute_check_root_view's root_override convention.
    missing_children = _missing_required_child_fields(root_id, draft_uuid)
    if missing or missing_children:
        return ("validation_error", missing, missing_children)
    state.draft_store["CheckRoots"].pop(draft_uuid, None)
    draft_row.pop("_child_snapshot", None)
    # [Fix] CheckItems/Barriers are real draft nodes now - this draft's own
    # tagged rows become the new active children (see
    # _activate_tagged_children); works uniformly for both an edit-draft
    # (deletes the old active-tagged rows first) and a create-draft (no
    # active-tagged rows exist yet, so that delete is a no-op).
    _activate_tagged_children(root_id, draft_uuid)
    if root_id in state.store["CheckRoots"]:
        active_row = state.store["CheckRoots"][root_id]
        for k, v in draft_row.items():
            if k not in ("RootId", "ActiveUUID", "DraftUUID", "IsActiveEntity"):
                active_row[k] = v
        active_row["LastChangedAt"] = odata_format.odata_date()
    else:
        draft_row["LastChangedAt"] = odata_format.odata_date()
        draft_row["ActiveUUID"] = root_id
        draft_row["DraftUUID"] = config.ZERO_GUID
        draft_row["IsActiveEntity"] = True
        state.store["CheckRoots"][root_id] = draft_row
    return resolvers.wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], state.store["CheckRoots"][root_id], None)


def _draft_discard(draft_uuid):
    """[Фаза 5] CheckRootDiscardAction — удаляет черновик draft_uuid без
    применения к активной записи. Если активной записи для этого черновика
    никогда не было (черновик из CREATE) — отменять нечего, кроме самого
    черновика. None, если DraftUUID не найден.

    [Fix] CheckBasics edits during the edit session are NOT draft-scoped
    (see _snapshot_children) - discard must roll those back, otherwise
    Cancel silently keeps Basic-proxy field changes the user just asked to
    undo. CheckItems/Barriers ARE real draft nodes now (Common.DraftNode) -
    their draft-tagged rows are simply dropped (_purge_tagged_children); the
    active-tagged rows were never touched in the first place, so there's
    nothing to restore for them. A create-only draft has no active twin to
    preserve at all, so its composition rows (CheckBasics AND every tagged
    CheckItem/Barrier) are purged outright instead."""
    draft_row = state.draft_store["CheckRoots"].pop(draft_uuid, None)
    if draft_row is None:
        return None
    root_id = draft_row["RootId"]
    if root_id not in state.store["CheckRoots"]:
        _purge_children(root_id)
        return "discarded-create-only"
    _restore_children(root_id, draft_row.get("_child_snapshot"))
    _purge_tagged_children(root_id, draft_uuid)
    return resolvers.wrap_entity("CheckRoots", config.ENTITY_TYPES["CheckRoots"], state.store["CheckRoots"][root_id], None)
