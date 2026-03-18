sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService"
], function (AnalyticsContracts, AnalyticsDrilldownRuntime, AnalyticsExportRuntime, NavigationIntentService) {
    "use strict";

    return {
        onCloseAnalytics: function (oController) {
            NavigationIntentService.navigateBackFromAnalytics(oController);
        },

        onCloseAnalyticsReportDialog: function (oController) {
            if (oController._oAnalyticsReportDialog) {
                oController._oAnalyticsReportDialog.close();
            }
        },

        onDrilldownAnalyticsBuilder: function (oController, oEvent, sDimension, sMetric) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sLabel = String((oPoint && (oPoint.Dimension || oPoint.label || oPoint.labelShort)) || "").trim();
            var mMap = {
                MONTH: "DateCheck",
                LPC: "Lpc",
                PROFESSION: "ProfessionText",
                LOCATION: "LocationKey",
                SOURCE: "SourceKey"
            };
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, mMap[sDimension], sLabel, {
                dimension: sDimension,
                metric: sMetric,
                monthLabel: sDimension === AnalyticsContracts.DIMENSIONS.MONTH ? sLabel : ""
            });
        },

        onDrilldownAnalyticsLocation: function (oController, oEvent, sMetric) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "LocationKey", String((oPoint && oPoint.Location) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LOCATION,
                metric: String(sMetric || "").trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsLpc: function (oController, oEvent, sMetric) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "Lpc", String((oPoint && oPoint.LPC) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LPC,
                metric: String(sMetric || "").trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsProfession: function (oController, oEvent, sMetric) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "ProfessionText", String((oPoint && oPoint.Profession) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.PROFESSION,
                metric: String(sMetric || "").trim().toUpperCase()
            });
        },

        onDrilldownAnalyticsSource: function (oController, oEvent, sMetric) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sSource = String((oPoint && oPoint.Source) || "").trim().toUpperCase();
            if (!sSource || sSource === "ALL") {
                return Promise.resolve(false);
            }
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "SourceKey", sSource, {
                dimension: AnalyticsContracts.DIMENSIONS.SOURCE,
                metric: String(sMetric || oPoint && oPoint.metric || "").trim().toUpperCase()
            });
        },

        onExportAnalyticsReport: function (oController) {
            return AnalyticsExportRuntime.exportAnalyticsReport(oController);
        },

        onOpenAnalyticsReportDialog: function (oController) {
            return AnalyticsExportRuntime.ensureAnalyticsReportDialog(oController).then(function (oDialog) {
                oDialog.open();
            });
        }
    };
});
