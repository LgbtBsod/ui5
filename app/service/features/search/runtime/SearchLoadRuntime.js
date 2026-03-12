sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFormattingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (ComponentFormattingRuntime, ControllerViewStateRuntime, ModelStateRuntime, StatePaths) {
    "use strict";

    function formatSearchDateTime(vDate) {
        if (vDate === null || vDate === undefined || vDate === "") {
            return "-";
        }
        return ComponentFormattingRuntime.formatHumanDateTime(vDate);
    }

    function setSettledViewState(oController, bCanExport, sWorkflowStage) {
        ControllerViewStateRuntime.setMany(oController, {
            "/busy": false,
            "/tableBusy": false,
            "/canExport": !!bCanExport,
            "/hasRows": !!bCanExport,
            "/workflowStage": String(sWorkflowStage || "DISCOVER"),
            "/lastUpdatedAt": formatSearchDateTime(new Date())
        });
    }

    function setLoadStatus(oController, mStatus) {
        var oStatus = mStatus || {};
        var mState = {
            "/loadError": !!oStatus.loadError,
            "/loadErrorMessage": oStatus.loadError ? String(oStatus.loadErrorMessage || "Search request failed") : ""
        };
        mState[StatePaths.UI_BUSY_SEARCH_TABLE] = !!oStatus.isBusy;
        ModelStateRuntime.setMany(oController, "state", mState);
    }

    function markLoading(oController) {
        ControllerViewStateRuntime.set(oController, "/tableBusy", true);
        setLoadStatus(oController, {
            isBusy: true,
            loadError: false
        });
    }

    function applyLoadError(oController, sErrorMessage) {
        setSettledViewState(oController, false, "REVIEW");
        setLoadStatus(oController, {
            isBusy: false,
            loadError: true,
            loadErrorMessage: sErrorMessage
        });
    }

    function applyLoadSuccess(oController, aRows) {
        var iCount = Array.isArray(aRows) ? aRows.length : 0;
        setSettledViewState(oController, iCount > 0, iCount > 0 ? "ANALYZE" : "DISCOVER");
        setLoadStatus(oController, {
            isBusy: false,
            loadError: false
        });
    }

    return {
        applyLoadError: applyLoadError,
        applyLoadSuccess: applyLoadSuccess,
        markLoading: markLoading,
        setLoadStatus: setLoadStatus,
        setSettledViewState: setSettledViewState
    };
});
