sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownRuntime"
], function (
    AnalyticsContracts,
    AnalyticsDrilldownRuntime
) {
    "use strict";

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

    function buildBuilderDrilldownRequest(oEvent, sBuilderDimension, sBuilderMetric) {
        var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
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

    return {
        buildBuilderDrilldownRequest: buildBuilderDrilldownRequest,
        getEventParameter: getEventParameter,
        getEventSource: getEventSource,
        resolveSelectedSource: resolveSelectedSource
    };
});
