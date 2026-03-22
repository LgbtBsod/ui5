sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFormattingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ComponentFormattingRuntime, ControllerViewStateRuntime, ModelStateRuntime, StatePaths, ProgressiveReadinessContracts, ModelContracts) {
    "use strict";

    var SEARCH_READINESS = ProgressiveReadinessContracts.SEARCH;
    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function formatSearchDateTime(vDate) {
        if (vDate === null || vDate === undefined || vDate === "") {
            return "-";
        }
        return ComponentFormattingRuntime.formatHumanDateTime(vDate);
    }

    function setSettledViewState(oController, bCanExport, sWorkflowStage, iResultCount) {
        ControllerViewStateRuntime.setMany(oController, {
            "/busy": false,
            "/tableBusy": false,
            "/canExport": !!bCanExport,
            "/hasRows": !!bCanExport,
            "/resultCount": Math.max(0, Number(iResultCount || 0)),
            "/workflowStage": String(sWorkflowStage || "DISCOVER"),
            "/lastUpdatedAt": formatSearchDateTime(new Date())
        });
    }

    function setLoadStatus(oController, mStatus) {
        var oStatus = mStatus || {};
        var mState = {
            "/loadError": !!oStatus.loadError,
            "/loadErrorMessage": oStatus.loadError ? String(oStatus.loadErrorMessage || SEARCH_READINESS.LOAD_ERROR_MESSAGE) : ""
        };
        mState[StatePaths.UI_BUSY_SEARCH_TABLE] = !!oStatus.isBusy;
        ModelStateRuntime.setMany(oController, STATE_MODEL, mState);
    }

    function markLoading(oController) {
        ControllerViewStateRuntime.set(oController, "/tableBusy", true);
        setLoadStatus(oController, {
            isBusy: true,
            loadError: false
        });
    }

    function applyLoadError(oController, sErrorMessage) {
        setSettledViewState(oController, false, "REVIEW", 0);
        setLoadStatus(oController, {
            isBusy: false,
            loadError: true,
            loadErrorMessage: sErrorMessage
        });
    }

    function applyLoadSuccess(oController, aRows) {
        var iCount = Array.isArray(aRows) ? aRows.length : 0;
        setSettledViewState(oController, iCount > 0, iCount > 0 ? "ANALYZE" : "DISCOVER", iCount);
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
