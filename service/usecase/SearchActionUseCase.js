sap.ui.define([], function () {
    "use strict";

    function extractSelectedChecklistId(oSelected) {
        return String((((oSelected || {}).root || {}).id) || "").trim();
    }

    function buildAnalyticsPayload(mFilterPayload) {
        return {
            filterId: mFilterPayload.filterId,
            filterLpc: mFilterPayload.filterLpc,
            filterFailedChecks: mFilterPayload.filterFailedChecks,
            filterFailedBarriers: mFilterPayload.filterFailedBarriers
        };
    }

    function buildExportRowsFromVisible(aRows) {
        return (aRows || []).map(function (oItem) {
            var oRoot = (oItem && oItem.root) || {};
            var oBasic = (oItem && oItem.basic) || {};
            return {
                id: oRoot.id || "",
                checklist_id: oBasic.checklist_id || "",
                status: oRoot.status || "",
                lpc: oBasic.LPC_TEXT || oBasic.LPC_KEY || "",
                observer: oBasic.OBSERVER_FULLNAME || "",
                observed: oBasic.OBSERVED_FULLNAME || ""
            };
        });
    }

    function shouldProceedAfterUnsavedDecision(sDecision) {
        return sDecision === "DISCARD" || sDecision === "SAVE";
    }

    return {
        extractSelectedChecklistId: extractSelectedChecklistId,
        buildAnalyticsPayload: buildAnalyticsPayload,
        buildExportRowsFromVisible: buildExportRowsFromVisible,
        shouldProceedAfterUnsavedDecision: shouldProceedAfterUnsavedDecision
    };
});
