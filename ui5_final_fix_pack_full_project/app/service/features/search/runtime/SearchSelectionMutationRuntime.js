sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity"
], function (ChecklistIdentity) {
    "use strict";

    function applySelectionState(oController, aSelectedRowIds, sSelectedRowDisplayId, sSource, fnSelectionChanged) {
        var aIds = ChecklistIdentity.normalizeChecklistIds(aSelectedRowIds);
        if (typeof fnSelectionChanged !== "function") {
            return Promise.resolve();
        }
        return fnSelectionChanged({
            selectedRowId: aIds[0] || "",
            selectedRowDisplayId: String(sSelectedRowDisplayId || "").trim(),
            selectedRowIds: aIds,
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
        clearSelection: function (oController, fnSelectionChanged) {
            return Promise.resolve(applySelectionState(oController, [], "", "clearSelection", fnSelectionChanged));
        },
        selectVisibleRows: selectVisibleRows
    };
});
