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
                LPC: "Lpc",
                PROFESSION: "ProfessionText",
                LOCATION: "LocationKey"
            };
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, mMap[sDimension], sLabel, {
                dimension: sDimension,
                metric: sMetric
            });
        },

        onDrilldownAnalyticsLocation: function (oController, oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "LocationKey", String((oPoint && oPoint.Location) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LOCATION
            });
        },

        onDrilldownAnalyticsLpc: function (oController, oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "Lpc", String((oPoint && oPoint.LPC) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.LPC
            });
        },

        onDrilldownAnalyticsProfession: function (oController, oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "ProfessionText", String((oPoint && oPoint.Profession) || "").trim(), {
                dimension: AnalyticsContracts.DIMENSIONS.PROFESSION
            });
        },

        onDrilldownAnalyticsSource: function (oController, oEvent) {
            var oPoint = AnalyticsDrilldownRuntime.extractDrilldownPayload(oEvent);
            var sSource = String((oPoint && oPoint.Source) || "").trim().toUpperCase();
            if (!sSource || sSource === "ALL") {
                return Promise.resolve(false);
            }
            return AnalyticsDrilldownRuntime.queueAnalyticsDrilldown(oController, "Source", sSource, {
                dimension: AnalyticsContracts.DIMENSIONS.SOURCE
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
