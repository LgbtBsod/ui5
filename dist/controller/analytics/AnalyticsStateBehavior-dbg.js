sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime"
], function (AnalyticsBuilderRuntime, ControllerViewStateRuntime) {
    "use strict";

    function applyComparisonMetricSelection(oController) {
        AnalyticsBuilderRuntime.applyComparisonMetricSelection(oController);
    }

    function applyBuilderSelection(oController, mOverrides) {
        AnalyticsBuilderRuntime.applyBuilderSelection(oController, mOverrides || {});
    }

    function syncAnalyticsContextHints(oController) {
        AnalyticsBuilderRuntime.syncAnalyticsContextHints(oController);
    }

    function setCompareYearValidation(oController, sState, sText) {
        ControllerViewStateRuntime.setMany(oController, {
            "/compareYearValueState": sState || "None",
            "/compareYearValueStateText": sText || ""
        });
    }

    return {
        applyBuilderSelection: applyBuilderSelection,
        applyComparisonMetricSelection: applyComparisonMetricSelection,
        setCompareYearValidation: setCompareYearValidation,
        syncAnalyticsContextHints: syncAnalyticsContextHints
    };
});
