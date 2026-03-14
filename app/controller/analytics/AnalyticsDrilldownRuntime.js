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
    var MONTH_INDEX = Object.freeze({
        JAN: 0,
        JANUARY: 0,
        FEB: 1,
        FEBRUARY: 1,
        MAR: 2,
        MARCH: 2,
        APR: 3,
        APRIL: 3,
        MAY: 4,
        JUN: 5,
        JUNE: 5,
        JUL: 6,
        JULY: 6,
        AUG: 7,
        AUGUST: 7,
        SEP: 8,
        SEPT: 8,
        SEPTEMBER: 8,
        OCT: 9,
        OCTOBER: 9,
        NOV: 10,
        NOVEMBER: 10,
        DEC: 11,
        DECEMBER: 11
    });

    function readSelectedSource(oController) {
        return String(ControllerViewStateRuntime.get(oController, "/selectedSource", AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toUpperCase();
    }

    function buildSearchDrilldownIntent(sFilterKey, sFilterValue, oController, mExtras) {
        var sSelectedYear = String(ControllerViewStateRuntime.get(oController, SELECTED_YEAR_PATH, "") || "").trim();
        var sCompareYear = String(ControllerViewStateRuntime.get(oController, COMPARE_YEAR_PATH, "") || "").trim();
        var sAnalyticsSource = readSelectedSource(oController);
        var sMonthLabel = String((mExtras && mExtras.monthLabel) || "").trim();
        return {
            source: TOKENS.ANALYTICS,
            filterKey: String(sFilterKey || "").trim(),
            filterValue: String(sFilterValue || "").trim(),
            selectedYear: sSelectedYear,
            compareYear: sCompareYear,
            analyticsSource: sAnalyticsSource,
            extras: Object.assign({}, mExtras || {}, {
                analyticsSource: sAnalyticsSource,
                selectedYear: sSelectedYear,
                compareYear: sCompareYear,
                monthLabel: sMonthLabel
            })
        };
    }

    function extractDrilldownPayload(oEvent) {
        var aData = oEvent && oEvent.getParameter && oEvent.getParameter("data");
        var oEntry = Array.isArray(aData) && aData.length ? aData[0] : null;
        var oPoint = oEntry && (oEntry.data || oEntry.dataContext || {});
        return oPoint || {};
    }

    function queueAnalyticsDrilldown(oController, sFilterKey, sFilterValue, mExtras) {
        var mResolvedExtras = Object.assign({}, mExtras || {});
        var sValue = String(sFilterValue || "").trim();
        var sFilter = String(sFilterKey || "").trim();
        var sSelectedYear = String(ControllerViewStateRuntime.get(oController, SELECTED_YEAR_PATH, "") || "").trim();
        var sAnalyticsSource = readSelectedSource(oController);
        if (mResolvedExtras.dimension === AnalyticsContracts.DIMENSIONS.MONTH) {
            mResolvedExtras.monthLabel = sValue;
        }
        if ((!sFilter || !sValue) && !sSelectedYear && (!sAnalyticsSource || sAnalyticsSource === AnalyticsContracts.SOURCES.ALL)) {
            return Promise.resolve(false);
        }
        ModelStateRuntime.write(oController, STATE_MODEL, ANALYTICS_DRILLDOWN_INTENT_PATH, buildSearchDrilldownIntent(sFilter, sValue, oController, mResolvedExtras));
        NavigationIntentService.navigateToSearch(oController);
        return Promise.resolve(true);
    }

    return {
        extractDrilldownPayload: extractDrilldownPayload,
        queueAnalyticsDrilldown: queueAnalyticsDrilldown,
        MONTH_INDEX: MONTH_INDEX
    };
});
