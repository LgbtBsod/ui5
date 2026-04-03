sap.ui.define([
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/FeedbackDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DialogConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsExportRows",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsViewStateReader",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/SpreadsheetExport",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime"
], function (Fragment, ControllerViewStateRuntime, FeedbackDefaultHandlers, AnalyticsContracts, DialogContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, AnalyticsExportRows, AnalyticsViewStateReader, SpreadsheetExport, DebugLogger, FeedbackConstants, MessageKeyConstants, MessageCodeConstants, ControllerTextRuntime) {
    "use strict";

    var PATHS = AnalyticsContracts.PATHS;
    var VIEW_MESSAGE_KEYS = MessageKeyConstants.VIEW;
    var ANALYTICS_CONTEXT_TEXT_KEY = "analyticsTitle";
    var TECHNICAL_CODES = Object.freeze({
        BUILD_ROWS_FAILED: MessageCodeConstants.TECHNICAL.ANALYTICS_EXPORT_ROWS_UNAVAILABLE,
        EXPORT_FAILED: MessageCodeConstants.TECHNICAL.ANALYTICS_EXPORT_FAILED
    });

    function getLoggerPayload(oError, sFallbackMessage) {
        return {
            code: String((oError && oError.code) || sFallbackMessage || TECHNICAL_CODES.EXPORT_FAILED),
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

    function showToast(oController, sTextKey, aArgs, sSeverity) {
        return FeedbackDefaultHandlers.handlers.showToast({
            controller: oController,
            textKey: sTextKey,
            args: aArgs || [],
            severity: sSeverity
        });
    }

    function analyticsContextLabel(oController) {
        return ControllerTextRuntime.getText(oController, ANALYTICS_CONTEXT_TEXT_KEY, [], "") || "";
    }

    function exportAnalyticsReport(oController) {
        var oBundle = getBundle(oController);
        var oViewState = ControllerViewStateRuntime.get(oController, "/", {});
        var aRows;
        var sErrorMessage;
        try {
            aRows = AnalyticsExportRows.buildRows(oViewState, oBundle);
        } catch (oError) {
            sErrorMessage = String((oError && oError.code) || TECHNICAL_CODES.BUILD_ROWS_FAILED);
            ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
            if (DebugLogger && typeof DebugLogger.error === "function") {
                DebugLogger.error("AnalyticsExportRuntime", "build_rows_failed", getLoggerPayload(oError, sErrorMessage));
            }
            showToast(oController, VIEW_MESSAGE_KEYS.EXPORT_FAILED, [analyticsContextLabel(oController)], FeedbackConstants.SEVERITY.ERROR);
            return Promise.resolve(false);
        }
        if (!aRows.length) {
            return showToast(oController, VIEW_MESSAGE_KEYS.EXPORT_EMPTY, [], FeedbackConstants.SEVERITY.WARNING);
        }
        try {
            return SpreadsheetExport.download(buildAnalyticsExportFileName(oController), aRows, {
                workbookColumns: buildSpreadsheetColumns(aRows)
            }).then(function () {
                showToast(oController, VIEW_MESSAGE_KEYS.EXPORT_SUCCESS, [], FeedbackConstants.SEVERITY.INFO);
                return true;
            }).catch(function (oError) {
                sErrorMessage = String((oError && oError.code) || TECHNICAL_CODES.EXPORT_FAILED);
                ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
                if (DebugLogger && typeof DebugLogger.error === "function") {
                    DebugLogger.error("AnalyticsExportRuntime", "export_failed", Object.assign({
                        export: getLoggerPayload(oError, sErrorMessage)
                    }, getSelectionLogContext(oController)));
                }
                showToast(oController, VIEW_MESSAGE_KEYS.EXPORT_FAILED, [analyticsContextLabel(oController)], FeedbackConstants.SEVERITY.ERROR);
                return false;
            });
        } catch (oError) {
            sErrorMessage = String((oError && oError.code) || TECHNICAL_CODES.EXPORT_FAILED);
            ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
            if (DebugLogger && typeof DebugLogger.error === "function") {
                DebugLogger.error("AnalyticsExportRuntime", "export_failed", Object.assign({
                    export: getLoggerPayload(oError, sErrorMessage)
                }, getSelectionLogContext(oController)));
            }
            showToast(oController, VIEW_MESSAGE_KEYS.EXPORT_FAILED, [analyticsContextLabel(oController)], FeedbackConstants.SEVERITY.ERROR);
            return Promise.resolve(false);
        }
    }

    return {
        ensureAnalyticsReportDialog: ensureAnalyticsReportDialog,
        exportAnalyticsReport: exportAnalyticsReport
    };
});
