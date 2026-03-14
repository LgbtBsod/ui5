sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (ControllerViewStateRuntime, ModelStateRuntime, NavigationIntentService, AnalyticsContracts, ModelContracts) {
    "use strict";

    var TOKENS = ModelContracts.TOKENS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var ANALYTICS_DRILLDOWN_INTENT_PATH = "/analyticsDrilldownIntent";
    var SELECTED_YEAR_PATH = "/selectedYear";
    var COMPARE_YEAR_PATH = "/compareYear";

    function readSelectedSource(oController) {
        return String(ControllerViewStateRuntime.get(oController, "/selectedSource", AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toUpperCase();
    }

    function buildSearchDrilldownIntent(sFilterKey, sFilterValue, oController, mExtras) {
        return {
            source: TOKENS.ANALYTICS,
            filterKey: String(sFilterKey || "").trim(),
            filterValue: String(sFilterValue || "").trim(),
            selectedYear: String(ControllerViewStateRuntime.get(oController, SELECTED_YEAR_PATH, "") || "").trim(),
            compareYear: String(ControllerViewStateRuntime.get(oController, COMPARE_YEAR_PATH, "") || "").trim(),
            analyticsSource: readSelectedSource(oController),
            extras: Object.assign({}, mExtras || {})
        };
    }

    function extractDrilldownPayload(oEvent) {
        var aData = oEvent && oEvent.getParameter && oEvent.getParameter("data");
        var oEntry = Array.isArray(aData) && aData.length ? aData[0] : null;
        var oPoint = oEntry && (oEntry.data || oEntry.dataContext || {});
        return oPoint || {};
    }

    function queueAnalyticsDrilldown(oController, sFilterKey, sFilterValue, mExtras) {
        var sValue = String(sFilterValue || "").trim();
        if (!sFilterKey || !sValue) {
            return Promise.resolve(false);
        }
        ModelStateRuntime.write(oController, STATE_MODEL, ANALYTICS_DRILLDOWN_INTENT_PATH, buildSearchDrilldownIntent(sFilterKey, sValue, oController, mExtras));
        NavigationIntentService.navigateToSearch(oController);
        return Promise.resolve(true);
    }

    return {
        extractDrilldownPayload: extractDrilldownPayload,
        queueAnalyticsDrilldown: queueAnalyticsDrilldown
    };
});
