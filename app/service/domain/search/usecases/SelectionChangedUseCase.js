sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSmartControlCoordinator"
], function (Result, Effects, ChecklistIdentity, SearchSelectionStateRuntime, SearchSmartControlCoordinator) {
    "use strict";

    function SelectionChangedUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

    function execute(mInput) {
        var aSelectedRowIds = [];
        var sSelectedRowId = "";
        var sSelectedRowDisplayId = "";
        var mSelectionState;
        if (mInput && Array.isArray(mInput.selectedRowIds)) {
            aSelectedRowIds = mInput.selectedRowIds.slice(0);
        }
        if (!aSelectedRowIds.length && mInput && mInput.event) {
            aSelectedRowIds = SearchSmartControlCoordinator.extractChecklistIdsFromSelectionEvent(mInput.event);
        }
        sSelectedRowId = String((mInput && mInput.selectedRowId) || "").trim();
        if (sSelectedRowId) {
            aSelectedRowIds.unshift(sSelectedRowId);
        }
        aSelectedRowIds = ChecklistIdentity.normalizeChecklistIds(aSelectedRowIds);
        sSelectedRowDisplayId = String((mInput && mInput.selectedRowDisplayId) || "").trim() || sSelectedRowId;
        mSelectionState = SearchSelectionStateRuntime.buildSelectionStatePayload(aSelectedRowIds, sSelectedRowDisplayId);

        return Promise.resolve(Result.ok({
            selectedRowId: mSelectionState.selectedRowId,
            selectedRowDisplayId: mSelectionState.selectedRowDisplayId,
            selectedRowIds: mSelectionState.selectedRowIds,
            selectionCount: mSelectionState.selectionCount,
            hasSelection: mSelectionState.hasSelection,
            canCopy: mSelectionState.canCopy
        }, [
            Effects.modelPatch("view", "/selectedRowId", mSelectionState.selectedRowId),
            Effects.modelPatch("view", "/selectedRowDisplayId", mSelectionState.selectedRowDisplayId),
            Effects.modelPatch("view", "/selectedRowIds", mSelectionState.selectedRowIds),
            Effects.modelPatch("view", "/selectionCount", mSelectionState.selectionCount),
            Effects.modelPatch("view", "/hasSelection", mSelectionState.hasSelection),
            Effects.modelPatch("view", "/canCopy", mSelectionState.canCopy),
            Effects.modelPatch("view", "/canDelete", mSelectionState.canDelete)
        ]));
    }

    return SelectionChangedUseCase;
});
