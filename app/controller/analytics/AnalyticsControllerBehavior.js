sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsYearBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsSelectionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsStateBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsReportBehavior"
], function (AnalyticsFacade, CtxFactory, AnalyticsContracts, AnalyticsYearBehavior, AnalyticsDrilldownBehavior, AnalyticsSelectionBehavior, AnalyticsRefreshBehavior, AnalyticsLifecycleBehavior, AnalyticsLoadBehavior, AnalyticsStateBehavior, AnalyticsReportBehavior) {
    "use strict";
    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var SELECTED_YEAR_PATH = "/selectedYear";
    var COMPARE_YEAR_PATH = "/compareYear";

    function buildCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    function coerceText(vValue) {
        return String(vValue || "").trim();
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

        _applyComparisonMetricSelection: function () {
            AnalyticsStateBehavior.applyComparisonMetricSelection(this);
        },

        _applyBuilderSelection: function (mOverrides) {
            AnalyticsStateBehavior.applyBuilderSelection(this, mOverrides);
        },

        _syncAnalyticsContextHints: function () {
            AnalyticsStateBehavior.syncAnalyticsContextHints(this);
        },

        _setCompareYearValidation: function (sState, sText) {
            AnalyticsStateBehavior.setCompareYearValidation(this, sState, sText);
        },

        _loadAnalytics: function (sReason) {
            return AnalyticsLoadBehavior.loadAnalytics(this, sReason, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH, buildCtx);
        },

        _pollRefreshStateUntilSettled: function (iAttemptsLeft) {
            return AnalyticsLoadBehavior.pollRefreshStateUntilSettled(this, iAttemptsLeft, buildCtx);
        },

        _onAnalyticsMatched: function () {
            AnalyticsLifecycleBehavior.onRouteEnter(this);
            return AnalyticsLoadBehavior.onAnalyticsMatched(this, function (oController, sReason) {
                return oController._loadAnalytics(sReason);
            });
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
                oController._applyComparisonMetricSelection();
            });
        },

        onSelectAnalyticsBuilderDimension: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsBuilderDimension(this, oEvent, function (oController, mOverrides) {
                oController._applyBuilderSelection(mOverrides);
            });
        },

        onSelectAnalyticsBuilderMetric: function (oEvent) {
            return AnalyticsSelectionBehavior.onSelectAnalyticsBuilderMetric(this, oEvent, function (oController, mOverrides) {
                oController._applyBuilderSelection(mOverrides);
            });
        },

        onApplyAnalyticsYearPreset: function (oEvent) {
            return AnalyticsYearBehavior.onApplyAnalyticsYearPreset(this, oEvent, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH, this._loadAnalytics.bind(this));
        },

        onDrilldownAnalyticsBuilder: function (oEvent) {
            var oViewModel = this.getModel && this.getModel("view");
            var sBuilderDimension = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty("/builderDimension") || AnalyticsContracts.BUILDER.FALLBACK_DIMENSION).trim().toUpperCase();
            var sBuilderMetric = String(oViewModel && oViewModel.getProperty && oViewModel.getProperty("/builderMetric") || "").trim().toUpperCase();
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsBuilder(
                this,
                oEvent,
                sBuilderDimension,
                sBuilderMetric
            );
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
            return AnalyticsReportBehavior.onOpenAnalyticsReportDialog(this);
        },

        onCloseAnalyticsReportDialog: function () {
            AnalyticsReportBehavior.onCloseAnalyticsReportDialog(this);
        },

        onExportAnalyticsReport: function () {
            return AnalyticsReportBehavior.onExportAnalyticsReport(this);
        },

        onCloseAnalytics: function () {
            AnalyticsReportBehavior.onCloseAnalytics(this);
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
