sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ControllerViewStateRuntime, ModelStateRuntime, StatePaths, ModelContracts) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;

    function openWorkflowAnalytics(oController, mHooks) {
        var fnNavigate = mHooks && mHooks.navigateToAnalytics;
        if (typeof fnNavigate === "function") {
            fnNavigate();
        }
        return Promise.resolve();
    }

    function closeWorkflowAnalytics(oController, mHooks) {
        var fnNavigateBack = mHooks && mHooks.navigateBackFromAnalytics;
        if (typeof fnNavigateBack === "function") {
            fnNavigateBack();
        }
    }

    function runExport(oController, sEntity, mHooks) {
        var fnExportFlow = mHooks && mHooks.exportFlow;
        var aSelectedRowIds = ControllerViewStateRuntime.get(oController, "/selectedRowIds", []) || [];
        ControllerViewStateRuntime.set(oController, "/exportBusy", true);
        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.UI_BUSY_EXPORT, true);
        return Promise.resolve(fnExportFlow({
            entity: sEntity || "screen",
            selectedRowIds: Array.isArray(aSelectedRowIds) ? aSelectedRowIds.slice(0) : []
        })).finally(function () {
            ControllerViewStateRuntime.set(oController, "/exportBusy", false);
            ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.UI_BUSY_EXPORT, false);
        });
    }

    return {
        closeWorkflowAnalytics: closeWorkflowAnalytics,
        openWorkflowAnalytics: openWorkflowAnalytics,
        runExport: runExport
    };
});
