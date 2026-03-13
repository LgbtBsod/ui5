sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsBuilderRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsYearRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsYearBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsDrilldownBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsLoadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsSelectionBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshBehavior"
], function (AnalyticsFacade, AnalyticsBuilderRuntime, CtxFactory, ControllerRouteRuntime, ControllerViewStateRuntime, AnalyticsContracts, ModelContracts, NavigationContracts, AnalyticsYearRuntime, AnalyticsYearBehavior, AnalyticsRefreshRuntime, AnalyticsDrilldownBehavior, AnalyticsLoadRuntime, AnalyticsSelectionBehavior, AnalyticsRefreshBehavior) {
    "use strict";
    var REFRESH_STATE_TASK_KEY = AnalyticsContracts.REFRESH.TASK_KEY;
    var SELECTED_YEAR_PATH = "/selectedYear";
    var COMPARE_YEAR_PATH = "/compareYear";

    function buildCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    return {
        onInit: function () {
            this._facade = new AnalyticsFacade();
            this._bAnalyticsInitialRouteHandled = false;
            ControllerViewStateRuntime.initModel(this, function () {
                return AnalyticsBuilderRuntime.createInitialViewState(REFRESH_STATE_TASK_KEY);
            });
            AnalyticsBuilderRuntime.applyBuilderSelection(this);
            ControllerRouteRuntime.attachMatched(this, [
                { name: NavigationContracts.ROUTES.ANALYTICS, handler: this._onAnalyticsMatched }
            ]);
        },

        onAfterRendering: function () {
            var sCurrentRouteName = String(this.getModel(ModelContracts.MODELS.STATE).getProperty("/currentRouteName") || "").trim();
            if (!this._bAnalyticsInitialRouteHandled && sCurrentRouteName === NavigationContracts.ROUTES.ANALYTICS) {
                this._onAnalyticsMatched();
            }
        },

        onExit: function () {
            ControllerRouteRuntime.detachAllMatched(this);
            if (this._oAnalyticsYearPicker && typeof this._oAnalyticsYearPicker.destroy === "function") {
                this._oAnalyticsYearPicker.destroy();
            }
            if (this._oAnalyticsReportDialog && typeof this._oAnalyticsReportDialog.destroy === "function") {
                this._oAnalyticsReportDialog.destroy();
            }
            this._oAnalyticsYearPicker = null;
            this._pAnalyticsYearPicker = null;
            this._oAnalyticsReportDialog = null;
            this._pAnalyticsReportDialog = null;
            this._facade = null;
            this._bAnalyticsInitialRouteHandled = null;
        },

        _applyComparisonMetricSelection: function () {
            AnalyticsBuilderRuntime.applyComparisonMetricSelection(this);
        },

        _applyBuilderSelection: function (mOverrides) {
            AnalyticsBuilderRuntime.applyBuilderSelection(this, mOverrides || {});
        },

        _syncAnalyticsContextHints: function () {
            AnalyticsBuilderRuntime.syncAnalyticsContextHints(this);
        },

        _setCompareYearValidation: function (sState, sText) {
            ControllerViewStateRuntime.setMany(this, {
                "/compareYearValueState": sState || "None",
                "/compareYearValueStateText": sText || ""
            });
        },

        _loadAnalytics: function (sReason) {
            return AnalyticsLoadRuntime.loadAnalytics(this, sReason, {
                applyBuilderSelection: function (oController) {
                    AnalyticsBuilderRuntime.applyBuilderSelection(oController);
                },
                applyComparisonMetricSelection: function (oController) {
                    AnalyticsBuilderRuntime.applyComparisonMetricSelection(oController);
                },
                buildCompareYearOptions: function (oController) {
                    return AnalyticsYearRuntime.buildCompareYearOptions(oController, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
                },
                buildCtx: buildCtx,
                buildYearOptions: function (oController) {
                    return AnalyticsYearRuntime.buildYearOptions(oController, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
                },
                setCompareYearValidation: function (oController, sState, sText) {
                    oController._setCompareYearValidation(sState, sText);
                },
                syncAnalyticsContextHints: function (oController) {
                    AnalyticsBuilderRuntime.syncAnalyticsContextHints(oController);
                },
                syncCompareYearDefaults: function (oController, sSelectedYear) {
                    return AnalyticsYearRuntime.syncCompareYearDefaults(oController, sSelectedYear, SELECTED_YEAR_PATH, COMPARE_YEAR_PATH);
                }
            });
        },

        _pollRefreshStateUntilSettled: function (iAttemptsLeft) {
            var oCtx = buildCtx(this);
            return AnalyticsRefreshRuntime.pollRefreshStateUntilSettled(this, iAttemptsLeft, function () {
                return oCtx && oCtx.analytics && oCtx.analytics.fetchRefreshState ? oCtx.analytics.fetchRefreshState() : null;
            });
        },

        _onAnalyticsMatched: function () {
            this._bAnalyticsInitialRouteHandled = true;
            return this._loadAnalytics("routeMatched");
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
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsBuilder(
                this,
                oEvent,
                String(ControllerViewStateRuntime.get(this, "/builderDimension", AnalyticsContracts.BUILDER.FALLBACK_DIMENSION) || AnalyticsContracts.BUILDER.FALLBACK_DIMENSION).trim().toUpperCase(),
                String(ControllerViewStateRuntime.get(this, "/builderMetric", "") || "").trim().toUpperCase()
            );
        },

        onDrilldownAnalyticsSource: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsSource(this, oEvent);
        },

        onDrilldownAnalyticsProfession: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsProfession(this, oEvent);
        },

        onDrilldownAnalyticsLpc: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsLpc(this, oEvent);
        },

        onDrilldownAnalyticsLocation: function (oEvent) {
            return AnalyticsDrilldownBehavior.onDrilldownAnalyticsLocation(this, oEvent);
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
        }
    };
});
