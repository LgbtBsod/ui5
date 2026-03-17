sap.ui.define([
    "sap/ui/core/Core",
    "sap/ui/core/Fragment",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsYearBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsSelectionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadBehavior"
], function (
    Core,
    Fragment,
    AnalyticsFacade,
    CtxFactory,
    AnalyticsBuilderRuntime,
    AnalyticsContracts,
    ModelContracts,
    ControllerViewStateRuntime,
    AnalyticsYearBehavior,
    AnalyticsDrilldownBehavior,
    AnalyticsSelectionBehavior,
    AnalyticsRefreshBehavior,
    AnalyticsLifecycleBehavior,
    AnalyticsLoadBehavior
) {
    "use strict";

    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var SELECTED_YEAR_PATH = "/selectedYear";
    var COMPARE_YEAR_PATH = "/compareYear";
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

    function buildCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    function coerceText(vValue) {
        return String(vValue || "").trim();
    }

    function ensureVizContentLoaded(oController) {
        var oHost = oController.byId && oController.byId("analyticsBreakdownsHost");
        if (!oHost) {
            return Promise.resolve(null);
        }
        if (oController._pAnalyticsBreakdownsContent) {
            return oController._pAnalyticsBreakdownsContent;
        }
        oController._pAnalyticsBreakdownsContent = Core.loadLibrary("sap.viz", { async: true }).then(function () {
            return Fragment.load({
                id: oController.getView().createId("analyticsBreakdownsFragment"),
                name: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.WorkflowAnalyticsBreakdowns",
                controller: oController
            });
        }).then(function (oFragment) {
            oHost.removeAllItems();
            oHost.addItem(oFragment);
            return oFragment;
        });
        return oController._pAnalyticsBreakdownsContent;
    }

    return {
        onInit: function () {
            AnalyticsLifecycleBehavior.onInit(this, new AnalyticsFacade(), REFRESH_STATE_TASK_KEY);
        },

        onAfterRendering: function () {
            AnalyticsLifecycleBehavior.onAfterRendering(this);
        },

        onExit: function () {
            AnalyticsLifecycleBehavior.onExit(this);
        },

        _setCompareYearValidation: function (sState, sText) {
            ControllerViewStateRuntime.setMany(this, {
                "/compareYearValueState": sState || "None",
                "/compareYearValueStateText": sText || ""
            });
        },

        _loadAnalytics: function (sReason) {
            return AnalyticsLoadBehavior.loadAnalytics(this, sReason, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH, buildCtx);
        },

        _pollRefreshStateUntilSettled: function (iAttemptsLeft) {
            return AnalyticsLoadBehavior.pollRefreshStateUntilSettled(this, iAttemptsLeft, buildCtx);
        },

        _onAnalyticsMatched: function () {
            AnalyticsLifecycleBehavior.onRouteEnter(this);
            return ensureVizContentLoaded(this).then(function () {
                return AnalyticsLoadBehavior.onAnalyticsMatched(this, function (oController, sReason) {
                    return oController._loadAnalytics(sReason);
                });
            }.bind(this));
        },

        _onAnalyticsRouteLeave: function () {
            AnalyticsLifecycleBehavior.onRouteLeave(this);
        },

        _startAnalyticsRefreshTimer: function () {
            AnalyticsLifecycleBehavior.startRefreshTimer(this);
        },

        onRefreshAnalytics: function () {
            return AnalyticsRefreshBehavior.onRefreshAnalytics(this, buildCtx, function (oController, iAttemptsLeft) {
                return oController._pollRefreshStateUntilSettled(iAttemptsLeft);
            }, function (oController, sReason) {
                return oController._loadAnalytics(sReason);
            });
        },

        onSelectAnalyticsYear: function (oEvent) {
            return AnalyticsYearBehavior.onSelectAnalyticsYear(this, oEvent, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onLiveChangeAnalyticsYear: function (oEvent) {
            AnalyticsYearBehavior.onLiveChangeAnalyticsYear(oEvent);
        },

        onSelectAnalyticsSource: function (oEvent) {
            return AnalyticsYearBehavior.onSelectAnalyticsSource(this, oEvent, this._loadAnalytics.bind(this));
        },

        onChangeAnalyticsCompareYear: function (oEvent) {
            return AnalyticsYearBehavior.onChangeAnalyticsCompareYear(this, oEvent, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onLiveChangeAnalyticsCompareYear: function (oEvent) {
            AnalyticsYearBehavior.onLiveChangeAnalyticsCompareYear(this, oEvent, this._setCompareYearValidation.bind(this));
        },

        onOpenAnalyticsSelectedYearPicker: function (oEvent) {
            return AnalyticsYearBehavior.onOpenAnalyticsSelectedYearPicker(this, oEvent);
        },

        onOpenAnalyticsCompareYearPicker: function (oEvent) {
            return AnalyticsYearBehavior.onOpenAnalyticsCompareYearPicker(this, oEvent);
        },

        onNavigateAnalyticsYearPickerBack: function () {
            AnalyticsYearBehavior.onNavigateAnalyticsYearPickerBack(this);
        },

        onNavigateAnalyticsYearPickerForward: function () {
            AnalyticsYearBehavior.onNavigateAnalyticsYearPickerForward(this);
        },

        onSelectAnalyticsYearFromPicker: function (oEvent) {
            return AnalyticsYearBehavior.onSelectAnalyticsYearFromPicker(this, oEvent, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH, this._setCompareYearValidation.bind(this), this._loadAnalytics.bind(this));
        },

        onSelectAnalyticsMetric: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsMetric(this, oEvent, function (oController) {
                AnalyticsBuilderRuntime.applyComparisonMetricSelection(oController);
            });
        },

        onSelectAnalyticsBuilderDimension: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsBuilderDimension(this, oEvent, function (oController, mOverrides) {
                AnalyticsBuilderRuntime.applyBuilderSelection(oController, mOverrides);
            });
        },

        onSelectAnalyticsBuilderMetric: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsBuilderMetric(this, oEvent, function (oController, mOverrides) {
                AnalyticsBuilderRuntime.applyBuilderSelection(oController, mOverrides);
            });
        },

        onApplyAnalyticsYearPreset: function (oEvent) {
            return AnalyticsYearBehavior.onApplyAnalyticsYearPreset(this, oEvent, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH, this._loadAnalytics.bind(this));
        },

        onDrilldownAnalyticsBuilder: function (oEvent) {
            var oViewModel = this.getModel && this.getModel(VIEW_MODEL);
            var sBuilderDimension = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty("/builderDimension") || AnalyticsContracts.BUILDER.FALLBACK_DIMENSION).trim().toUpperCase();
            var sBuilderMetric = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty("/builderMetric") || "").trim().toUpperCase();
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsBuilder(this, oEvent, sBuilderDimension, sBuilderMetric);
        },

        onDrilldownAnalyticsSource: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsSource(this, oEvent, AnalyticsContracts.METRICS.TOTAL);
        },

        onDrilldownAnalyticsSourceFailedChecks: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsSource(this, oEvent, AnalyticsContracts.METRICS.FAILED_CHECKS);
        },

        onDrilldownAnalyticsSourceFailedBarriers: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsSource(this, oEvent, AnalyticsContracts.METRICS.FAILED_BARRIERS);
        },

        onDrilldownAnalyticsProfession: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsProfession(this, oEvent, AnalyticsContracts.METRICS.FAILED_CHECKS);
        },

        onDrilldownAnalyticsLpc: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsLpc(this, oEvent, AnalyticsContracts.METRICS.FAILED_BARRIERS);
        },

        onDrilldownAnalyticsLocation: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsLocation(this, oEvent, AnalyticsContracts.METRICS.FAILED_CHECKS);
        },

        onOpenAnalyticsReportDialog: function () {
            return AnalyticsDrilldownBehavior.onOpenAnalyticsReportDialog(this);
        },

        onCloseAnalyticsReportDialog: function () {
            AnalyticsDrilldownBehavior.onCloseAnalyticsReportDialog(this);
        },

        onExportAnalyticsReport: function () {
            return AnalyticsDrilldownBehavior.onExportAnalyticsReport(this);
        },

        onCloseAnalytics: function () {
            AnalyticsDrilldownBehavior.onCloseAnalytics(this);
        },

        formatRefreshStatusState: function (sStatus, bIsRunning) {
            var sNormalizedStatus = coerceText(sStatus).toUpperCase();
            if (sNormalizedStatus === "ERROR") {
                return "Error";
            }
            if (sNormalizedStatus === "REQUESTED" || bIsRunning) {
                return "Warning";
            }
            return "Success";
        },

        formatRefreshEnabled: function (bRefreshBusy, bIsRunning, sStatus) {
            var sNormalizedStatus = coerceText(sStatus).toUpperCase();
            return !bRefreshBusy && !bIsRunning && sNormalizedStatus !== "REQUESTED";
        },

        formatRefreshStatusText: function (sStatus, bIsRunning, sLastError, sLastSuccessAt, sQueuedText, sRunningText, sUpdatedText, sIdleText) {
            var sNormalizedStatus = coerceText(sStatus).toUpperCase();
            var sResolvedError = coerceText(sLastError);
            var sResolvedSuccessAt = coerceText(sLastSuccessAt);
            if (sNormalizedStatus === "REQUESTED") {
                return coerceText(sQueuedText);
            }
            if (bIsRunning) {
                return coerceText(sRunningText);
            }
            if (sResolvedError) {
                return sResolvedError;
            }
            if (sResolvedSuccessAt) {
                return coerceText(sUpdatedText) + ": " + sResolvedSuccessAt;
            }
            return coerceText(sIdleText);
        }
    };
});
