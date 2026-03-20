sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsUiConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/YearValue"
], function (ControllerViewStateRuntime, AnalyticsContracts, AnalyticsUiContracts, YearValue) {
    "use strict";

    var PATHS = AnalyticsUiContracts.PATHS;

    function readSelectionState(oController) {
        return {
            selectedYear: YearValue.toTrimmedString(ControllerViewStateRuntime.get(oController, PATHS.SELECTED_YEAR, "")),
            compareYear: YearValue.toTrimmedString(ControllerViewStateRuntime.get(oController, PATHS.COMPARE_YEAR, "")),
            selectedSource: YearValue.toTrimmedString(
                ControllerViewStateRuntime.get(oController, PATHS.SELECTED_SOURCE, AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL
            ).toUpperCase()
        };
    }

    function readAnalyticsRoot(oController) {
        return ControllerViewStateRuntime.get(oController, PATHS.ANALYTICS, {}) || {};
    }

    return Object.freeze({
        readAnalyticsRoot: readAnalyticsRoot,
        readSelectionState: readSelectionState
    });
});
