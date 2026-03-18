sap.ui.define([
    "sap/ui/core/Fragment",
    "sap/ui/export/Spreadsheet",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DialogContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ReadinessTelemetryContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsExportRows",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsViewStateReader",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger"
], function (Fragment, Spreadsheet, ControllerViewStateRuntime, FeedbackCoordinator, AnalyticsContracts, AnalyticsUiContracts, DialogContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, AnalyticsExportRows, AnalyticsViewStateReader, DebugLogger) {
    "use strict";

    var PATHS = AnalyticsUiContracts.PATHS;

    function getLoggerPayload(oError, sFallbackMessage) {
        return {
            message: String((oError && oError.message) || sFallbackMessage || "analytics_export_failed"),
            stack: String((oError && oError.stack) || "")
        };
    }

    function getBundle(oController) {
        var oOwner = oController && oController.getOwnerComponent && oController.getOwnerComponent();
        var oModel = oOwner && oOwner.getModel && oOwner.getModel("i18n");
        return oModel && oModel.getResourceBundle ? oModel.getResourceBundle() : null;
    }

    function ensureAnalyticsReportDialog(oController) {
        if (oController._pAnalyticsReportDialog) {
            return oController._pAnalyticsReportDialog;
        }
        oController._pAnalyticsReportDialog = Fragment.load({
            id: oController.getView().createId("analyticsReportDialog"),
            name: DialogContracts.getFragmentName(DialogContracts.IDS.ANALYTICS_REPORT),
            controller: oController
        }).then(function (oDialog) {
            oController.getView().addDependent(oDialog);
            oController._oAnalyticsReportDialog = oDialog;
            ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.DEFERRED_DIALOG_READY, {
                dialog: "analyticsReport"
            });
            return oDialog;
        });
        return oController._pAnalyticsReportDialog;
    }

    function buildAnalyticsExportFileName(oController) {
        var oSelectionState = AnalyticsViewStateReader.readSelectionState(oController);
        return ["analytics", oSelectionState.selectedSource.toLowerCase() || "all", oSelectionState.selectedYear || "scope"].join("_");
    }

    function buildSpreadsheetColumns(aRows) {
        var aKeys = aRows.length ? Object.keys(aRows[0]) : [];
        return aKeys.map(function (sKey) {
            return {
                label: sKey,
                property: sKey,
                type: "string"
            };
        });
    }

    function getSelectionLogContext(oController) {
        var oSelectionState = AnalyticsViewStateReader.readSelectionState(oController);
        return {
            selectedSource: oSelectionState.selectedSource || AnalyticsContracts.SOURCES.ALL,
            selectedYear: oSelectionState.selectedYear || ""
        };
    }

    function exportAnalyticsReport(oController) {
        var oBundle = getBundle(oController);
        var oViewState = ControllerViewStateRuntime.get(oController, "/", {});
        var aRows;
        var oSpreadsheet;
        var sErrorMessage;
        try {
            aRows = AnalyticsExportRows.buildRows(oViewState, oBundle);
        } catch (oError) {
            sErrorMessage = String((oError && oError.message) || "Analytics export data is unavailable");
            ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
            if (DebugLogger && typeof DebugLogger.error === "function") {
                DebugLogger.error("AnalyticsExportRuntime", "build_rows_failed", getLoggerPayload(oError, sErrorMessage));
            }
            FeedbackCoordinator.showToast(oController, "exportFailed", ["analytics"], "error");
            return Promise.resolve(false);
        }
        if (!aRows.length) {
            return FeedbackCoordinator.showToast(oController, "nothingToExport", [], "warning");
        }
        try {
            oSpreadsheet = new Spreadsheet({
                workbook: {
                    columns: buildSpreadsheetColumns(aRows)
                },
                dataSource: aRows,
                fileName: buildAnalyticsExportFileName(oController) + ".xlsx"
            });
            return oSpreadsheet.build().then(function () {
                FeedbackCoordinator.showToast(oController, "searchExportSuccess", [], "info");
                return true;
            }).catch(function (oError) {
                sErrorMessage = String((oError && oError.message) || "Analytics export failed");
                ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
                if (DebugLogger && typeof DebugLogger.error === "function") {
                    DebugLogger.error("AnalyticsExportRuntime", "export_failed", Object.assign({
                        export: getLoggerPayload(oError, sErrorMessage)
                    }, getSelectionLogContext(oController)));
                }
                FeedbackCoordinator.showToast(oController, "exportFailed", ["analytics"], "error");
                return false;
            }).finally(function () {
                if (oSpreadsheet && typeof oSpreadsheet.destroy === "function") {
                    oSpreadsheet.destroy();
                }
            });
        } catch (oError) {
            sErrorMessage = String((oError && oError.message) || "Analytics export failed");
            ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
            if (DebugLogger && typeof DebugLogger.error === "function") {
                DebugLogger.error("AnalyticsExportRuntime", "export_failed", Object.assign({
                    export: getLoggerPayload(oError, sErrorMessage)
                }, getSelectionLogContext(oController)));
            }
            FeedbackCoordinator.showToast(oController, "exportFailed", ["analytics"], "error");
            return Promise.resolve(false);
        }
    }

    return {
        ensureAnalyticsReportDialog: ensureAnalyticsReportDialog,
        exportAnalyticsReport: exportAnalyticsReport
    };
});
