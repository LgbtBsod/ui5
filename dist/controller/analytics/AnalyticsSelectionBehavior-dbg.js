sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime"
], function (AnalyticsBuilderRuntime, ControllerViewStateRuntime) {
    "use strict";

    function onSelectAnalyticsMetric(oController, oEvent, fnApplyComparisonMetricSelection) {
        var sMetric = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
        if (!sMetric) {
            return;
        }
        ControllerViewStateRuntime.set(oController, "/comparisonMetric", sMetric);
        fnApplyComparisonMetricSelection(oController);
    }

    function onSelectAnalyticsBuilderDimension(oController, oEvent, fnApplyBuilderSelection) {
        var sDimension = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
        if (!sDimension) {
            return;
        }
        fnApplyBuilderSelection(oController, { dimension: sDimension });
    }

    function onSelectAnalyticsBuilderMetric(oController, oEvent, fnApplyBuilderSelection) {
        var sMetric = AnalyticsBuilderRuntime.getSelectedKeyFromEvent(oEvent);
        if (!sMetric) {
            return;
        }
        fnApplyBuilderSelection(oController, { metric: sMetric });
    }

    return {
        onSelectAnalyticsBuilderDimension: onSelectAnalyticsBuilderDimension,
        onSelectAnalyticsBuilderMetric: onSelectAnalyticsBuilderMetric,
        onSelectAnalyticsMetric: onSelectAnalyticsMetric
    };
});
