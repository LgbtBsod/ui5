sap.ui.define([
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DialogContracts",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/ExcelExport",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsExportRows"
], function (Fragment, ControllerViewStateRuntime, FeedbackCoordinator, AnalyticsContracts, DialogContracts, ExcelExport, AnalyticsExportRows) {
    "use strict";

    function ensureAnalyticsReportDialog(oController) {
        if (oController._pAnalyticsReportDialog) {
            return oController._pAnalyticsReportDialog;
        }
        oController._pAnalyticsReportDialog = Fragment.load({
            id: oController.getView().getId(),
            name: DialogContracts.getFragmentName(DialogContracts.IDS.ANALYTICS_REPORT),
            controller: oController
        }).then(function (oDialog) {
            oController.getView().addDependent(oDialog);
            oController._oAnalyticsReportDialog = oDialog;
            return oDialog;
        });
        return oController._pAnalyticsReportDialog;
    }

    function buildAnalyticsExportFileName(oController) {
        var sSource = String(ControllerViewStateRuntime.get(oController, "/selectedSource", AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toLowerCase();
        var sYear = String(ControllerViewStateRuntime.get(oController, "/selectedYear", "") || "").trim();
        return ["analytics", sSource || "all", sYear || "scope"].join("_");
    }

    function exportAnalyticsReport(oController) {
        var oBundle = oController.getOwnerComponent().getModel("i18n").getResourceBundle();
        var oViewState = ControllerViewStateRuntime.get(oController, "/", {});
        var aRows = AnalyticsExportRows.buildRows(oViewState, oBundle);
        if (!aRows.length) {
            return FeedbackCoordinator.showToast(oController, "nothingToExport", [], "warning");
        }
        try {
            ExcelExport.download(buildAnalyticsExportFileName(oController), aRows);
            FeedbackCoordinator.showToast(oController, "searchExportSuccess", [], "info");
        } catch (_oError) {
            FeedbackCoordinator.showToast(oController, "exportFailed", ["analytics"], "error");
        }
        return Promise.resolve(true);
    }

    return {
        ensureAnalyticsReportDialog: ensureAnalyticsReportDialog,
        exportAnalyticsReport: exportAnalyticsReport
    };
});
