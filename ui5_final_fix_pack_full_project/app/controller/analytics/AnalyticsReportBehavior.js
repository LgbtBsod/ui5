sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownBehavior"
], function (AnalyticsDrilldownBehavior) {
    "use strict";

    return {
        onCloseAnalytics: function (oController) {
            AnalyticsDrilldownBehavior.onCloseAnalytics(oController);
        },
        onCloseAnalyticsReportDialog: function (oController) {
            AnalyticsDrilldownBehavior.onCloseAnalyticsReportDialog(oController);
        },
        onExportAnalyticsReport: function (oController) {
            return AnalyticsDrilldownBehavior.onExportAnalyticsReport(oController);
        },
        onOpenAnalyticsReportDialog: function (oController) {
            return AnalyticsDrilldownBehavior.onOpenAnalyticsReportDialog(oController);
        }
    };
});
