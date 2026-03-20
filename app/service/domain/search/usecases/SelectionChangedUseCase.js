sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
"PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSmartControlCoordinator"
], function (Result, Effects, ChecklistIdentity, SearchSmartControlCoordinator) {
    "use strict";

    function SelectionChangedUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput) {
        var aSelectedRowIds = [];
        var sSelectedRowId = "";
        var sSelectedRowDisplayId = "";
        var iSelectionCount = 0;
        var bHasSelection = false;
        var bSingleSelection = false;
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
        sSelectedRowId = aSelectedRowIds[0] || "";
        sSelectedRowDisplayId = String((mInput && mInput.selectedRowDisplayId) || "").trim() || sSelectedRowId;
        iSelectionCount = aSelectedRowIds.length;
        bHasSelection = iSelectionCount > 0;
        bSingleSelection = iSelectionCount === 1;

        return Promise.resolve(Result.ok({
            selectedRowId: sSelectedRowId,
            selectedRowDisplayId: sSelectedRowDisplayId,
            selectedRowIds: aSelectedRowIds,
            selectionCount: iSelectionCount,
            hasSelection: bHasSelection,
            canCopy: bSingleSelection
        }, [
            Effects.modelPatch("view", "/selectedRowId", sSelectedRowId),
            Effects.modelPatch("view", "/selectedRowDisplayId", sSelectedRowDisplayId),
            Effects.modelPatch("view", "/selectedRowIds", aSelectedRowIds),
            Effects.modelPatch("view", "/selectionCount", iSelectionCount),
            Effects.modelPatch("view", "/hasSelection", bHasSelection),
            Effects.modelPatch("view", "/canCopy", bSingleSelection),
            Effects.modelPatch("view", "/canDelete", bSingleSelection)
        ]));
    }

    return SelectionChangedUseCase;
});