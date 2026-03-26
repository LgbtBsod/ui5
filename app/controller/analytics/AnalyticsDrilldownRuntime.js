sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsMonthRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsViewStateReader"
], function (ModelStateRuntime, NavigationIntentService, AnalyticsContracts, ModelContracts, AnalyticsMonthRuntime, AnalyticsViewStateReader) {
    "use strict";

    var PATHS = AnalyticsContracts.PATHS;
    var TOKENS = ModelContracts.TOKENS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var BUILDER_DRILLDOWN_MAP = {
        MONTH: "DateCheck",
        LPC: "Lpc",
        PROFESSION: "ProfessionText",
        LOCATION: "LocationKey",
        SOURCE: "SourceKey"
    };

    function getEventSource(oEvent) {
        return oEvent && oEvent.getSource ? oEvent.getSource() : null;
    }

    function getEventParameter(oEvent, sName) {
        return oEvent && oEvent.getParameter ? oEvent.getParameter(sName) : undefined;
    }

    function resolveSelectedSource(oEvent) {
        return String(
            getEventParameter(oEvent, "selectedKey") ||
            getEventSource(oEvent) && getEventSource(oEvent).getSelectedKey && getEventSource(oEvent).getSelectedKey() ||
            ""
        ).trim().toUpperCase();
    }

    function buildSearchDrilldownIntent(sFilterKey, sFilterValue, oController, mExtras) {
        var oSelectionState = AnalyticsViewStateReader.readSelectionState(oController);
        var sMonthLabel = String((mExtras && mExtras.monthLabel) || "").trim();

        return {
            source: TOKENS.ANALYTICS,
            filterKey: String(sFilterKey || "").trim(),
            filterValue: String(sFilterValue || "").trim(),
            selectedYear: oSelectionState.selectedYear,
            compareYear: oSelectionState.compareYear,
            analyticsSource: oSelectionState.selectedSource,
            extras: Object.assign({}, mExtras || {}, {
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

    function buildBuilderDrilldownRequest(oEvent, sBuilderDimension, sBuilderMetric) {
        var oPoint = extractDrilldownPayload(oEvent);
        var sLabel = String((oPoint && (oPoint.Dimension || oPoint.label || oPoint.labelShort)) || "").trim();

        return {
            filterKey: BUILDER_DRILLDOWN_MAP[sBuilderDimension],
            filterValue: sLabel,
            payload: {
                dimension: sBuilderDimension,
                metric: sBuilderMetric,
                monthLabel: sBuilderDimension === AnalyticsContracts.DIMENSIONS.MONTH ? sLabel : ""
            }
        };
    }

    function queueAnalyticsDrilldown(oController, sFilterKey, sFilterValue, mExtras) {
        var mResolvedExtras = Object.assign({}, mExtras || {});
        var sValue = String(sFilterValue || "").trim();
        var sFilter = String(sFilterKey || "").trim();
        var oSelectionState = AnalyticsViewStateReader.readSelectionState(oController);

        if (mResolvedExtras.dimension === AnalyticsContracts.DIMENSIONS.MONTH) {
            mResolvedExtras.monthLabel = sValue;
        }
        if ((!sFilter || !sValue) && !oSelectionState.selectedYear && (!oSelectionState.selectedSource || oSelectionState.selectedSource === AnalyticsContracts.SOURCES.ALL)) {
            return Promise.resolve(false);
        }
        ModelStateRuntime.write(
            oController,
            STATE_MODEL,
            PATHS.ANALYTICS_DRILLDOWN_INTENT,
            buildSearchDrilldownIntent(sFilter, sValue, oController, mResolvedExtras)
        );
        NavigationIntentService.navigateToSearch(oController);
        return Promise.resolve(true);
    }

    return {
        buildBuilderDrilldownRequest: buildBuilderDrilldownRequest,
        extractDrilldownPayload: extractDrilldownPayload,
        getEventParameter: getEventParameter,
        getEventSource: getEventSource,
        queueAnalyticsDrilldown: queueAnalyticsDrilldown,
        resolveSelectedSource: resolveSelectedSource,
        MONTH_INDEX: AnalyticsMonthRuntime.MONTH_INDEX
    };
});
