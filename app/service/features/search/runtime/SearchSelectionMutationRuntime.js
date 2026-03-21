sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity"
], function (ChecklistIdentity) {
    "use strict";

    function buildSelectionStatePayload(aSelectedRowIds, sSelectedRowDisplayId) {
        var aIds = ChecklistIdentity.normalizeChecklistIds(aSelectedRowIds);
        var sPrimaryId = aIds[0] || "";
        var sDisplayId = String(sSelectedRowDisplayId || "").trim() || sPrimaryId;
        var iSelectionCount = aIds.length;
        var bHasSelection = iSelectionCount > 0;
        var bSingleSelection = iSelectionCount === 1;

        return {
            selectedRowId: sPrimaryId,
            selectedRowDisplayId: bHasSelection ? sDisplayId : "",
            selectedRowIds: aIds,
            selectionCount: iSelectionCount,
            hasSelection: bHasSelection,
            canCopy: bSingleSelection,
            canDelete: bSingleSelection
        };
    }

    function applySelectionState(oController, aSelectedRowIds, sSelectedRowDisplayId, sSource, fnSelectionChanged) {
        var mSelectionState = buildSelectionStatePayload(aSelectedRowIds, sSelectedRowDisplayId);
        if (typeof fnSelectionChanged !== "function") {
            return Promise.resolve();
        }
        return fnSelectionChanged({
            selectedRowId: mSelectionState.selectedRowId,
            selectedRowDisplayId: mSelectionState.selectedRowDisplayId,
            selectedRowIds: mSelectionState.selectedRowIds,
            source: sSource || "selectionRuntime"
        });
    }

    function selectVisibleRows(oController, oInnerTable, aSelectedRowIds, sSelectedRowDisplayId, fnSelectionChanged) {
        return Promise.resolve(
            applySelectionState(
                oController,
                aSelectedRowIds,
                sSelectedRowDisplayId,
                "selectVisibleRows",
                fnSelectionChanged
            )
        ).then(function () {
            return {
                count: aSelectedRowIds.length,
                selectedRowIds: aSelectedRowIds
            };
        });
    }

    return {
        applySelectionState: applySelectionState,
        buildSelectionStatePayload: buildSelectionStatePayload,
        clearSelection: function (oController, fnSelectionChanged) {
            return Promise.resolve(applySelectionState(oController, [], "", "clearSelection", fnSelectionChanged));
        },
        selectVisibleRows: selectVisibleRows
    };
});
