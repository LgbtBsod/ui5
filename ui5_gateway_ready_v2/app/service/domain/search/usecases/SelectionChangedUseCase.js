sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/util/SearchSmartControlCoordinator"
], function (UseCase, Result, Effects, SearchSmartControlCoordinator) {
    "use strict";

    function SelectionChangedUseCase() {
        UseCase.call(this, "SelectionChangedUseCase");
    }

    SelectionChangedUseCase.prototype = Object.create(UseCase.prototype);
    SelectionChangedUseCase.prototype.constructor = SelectionChangedUseCase;

    function normalizeChecklistIds(aIds) {
        var mSeen = {};
        return (aIds || []).reduce(function (aAcc, sId) {
            var sNorm = String(sId || "").trim();
            if (!sNorm || mSeen[sNorm]) {
                return aAcc;
            }
            mSeen[sNorm] = true;
            aAcc.push(sNorm);
            return aAcc;
        }, []);
    }

    SelectionChangedUseCase.prototype.execute = function (mInput) {
        var aSelectedRowIds = [];
        var sSelectedRowId = "";
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
        aSelectedRowIds = normalizeChecklistIds(aSelectedRowIds);
        sSelectedRowId = aSelectedRowIds[0] || "";
        iSelectionCount = aSelectedRowIds.length;
        bHasSelection = iSelectionCount > 0;
        bSingleSelection = iSelectionCount === 1;

        return Promise.resolve(Result.ok({
            selectedRowId: sSelectedRowId,
            selectedRowIds: aSelectedRowIds,
            selectionCount: iSelectionCount,
            hasSelection: bHasSelection,
            canCopy: bSingleSelection
        }, [
            Effects.modelPatch("view", "/selectedRowId", sSelectedRowId),
            Effects.modelPatch("view", "/selectedRowIds", aSelectedRowIds),
            Effects.modelPatch("view", "/selectionCount", iSelectionCount),
            Effects.modelPatch("view", "/hasSelection", bHasSelection),
            Effects.modelPatch("view", "/canCopy", bSingleSelection),
            Effects.modelPatch("view", "/canDelete", bSingleSelection)
        ]));
    };

    return SelectionChangedUseCase;
});
