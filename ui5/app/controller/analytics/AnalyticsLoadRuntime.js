sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ReadinessTelemetryContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/YearValue"
], function (
    ControllerViewStateRuntime,
    ModelStateRuntime,
    FacadeCommandRuntime,
    StatePaths,
    AnalyticsContracts,
    AnalyticsUiContracts,
    ReadinessTelemetryContracts,
    ReadinessTelemetryRuntime,
    ModelContracts,
    YearValue
) {
    "use strict";

    var STATE_MODEL = ModelContracts.MODELS.STATE;
    var PATHS = AnalyticsUiContracts.PATHS;
    var MESSAGES = AnalyticsUiContracts.MESSAGES;
    var LOAD_REASONS = AnalyticsUiContracts.LOAD_REASONS;

    function setReadinessState(oController, sStatus, bReady, sReadyAt, sError) {
        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.READINESS_ANALYTICS, {
            status: sStatus,
            ready: bReady,
            readyAt: sReadyAt || "",
            error: sError || ""
        });
    }

    function setControllerBusy(oController, bBusy, sErrorMessage) {
        var oPatch = {};
        oPatch[PATHS.BUSY] = !!bBusy;
        if (typeof sErrorMessage === "string") {
            oPatch[PATHS.ERROR] = sErrorMessage;
        }
        ControllerViewStateRuntime.setMany(oController, oPatch);
        ModelStateRuntime.write(oController, STATE_MODEL, StatePaths.UI_BUSY_ANALYTICS, !!bBusy);
    }

    return {
        loadAnalytics: function (oController, sReason, mHooks) {
            var sSelectedYear = String(ControllerViewStateRuntime.get(oController, PATHS.SELECTED_YEAR, "") || "").trim();
            var sCompareYear = String(ControllerViewStateRuntime.get(oController, PATHS.COMPARE_YEAR, "") || "").trim();
            var sSelectedSource = String(ControllerViewStateRuntime.get(oController, PATHS.SELECTED_SOURCE, AnalyticsContracts.SOURCES.ALL) || AnalyticsContracts.SOURCES.ALL).trim().toUpperCase();
            var iSelectedYear = YearValue.parseYearOrNull(sSelectedYear);
            var iCompareYear = YearValue.parseYearOrNull(sCompareYear);
            var sResolvedReason = String(sReason || LOAD_REASONS.MANUAL || "manual");
            var sReadyAt = "";

            if (iSelectedYear === null) {
                ControllerViewStateRuntime.set(oController, PATHS.ERROR, MESSAGES.INVALID_YEAR);
                setReadinessState(oController, "error", false, "", MESSAGES.INVALID_YEAR);
                return Promise.resolve(false);
            }

            setReadinessState(oController, "loading", false, "", "");
            setControllerBusy(oController, true, "");

            return FacadeCommandRuntime.executeRaw(
                oController,
                oController._facade,
                "load",
                {
                    reason: sResolvedReason,
                    selectedYear: iSelectedYear,
                    compareYear: iCompareYear,
                    selectedSource: sSelectedSource
                },
                mHooks.buildCtx(oController)
            ).then(function (oResult) {
                var oAnalytics = ControllerViewStateRuntime.get(oController, PATHS.ANALYTICS, {}) || {};
                sReadyAt = new Date().toISOString();

                if (Array.isArray(oAnalytics.availableYears) && oAnalytics.availableYears.length) {
                    ControllerViewStateRuntime.set(oController, PATHS.AVAILABLE_YEARS, oAnalytics.availableYears);
                }
                if (oAnalytics.selectedYear) {
                    ControllerViewStateRuntime.set(oController, PATHS.SELECTED_YEAR, String(oAnalytics.selectedYear));
                }
                if (oAnalytics.compareYear) {
                    ControllerViewStateRuntime.set(oController, PATHS.COMPARE_YEAR, String(oAnalytics.compareYear));
                } else if (oAnalytics.selectedYear) {
                    mHooks.syncCompareYearDefaults(oController, String(oAnalytics.selectedYear));
                }
                if (oAnalytics.source) {
                    ControllerViewStateRuntime.set(oController, PATHS.SELECTED_SOURCE, String(oAnalytics.source));
                }
                if (oAnalytics.refreshState) {
                    ControllerViewStateRuntime.set(oController, PATHS.REFRESH_STATE, oAnalytics.refreshState);
                }

                ControllerViewStateRuntime.set(oController, PATHS.AVAILABLE_YEARS, mHooks.buildYearOptions(oController));
                ControllerViewStateRuntime.set(oController, PATHS.COMPARE_YEAR_OPTIONS, mHooks.buildCompareYearOptions(oController));
                mHooks.setCompareYearValidation(oController, AnalyticsUiContracts.VALIDATION_STATES.NONE, "");
                mHooks.applyComparisonMetricSelection(oController);
                mHooks.applyBuilderSelection(oController);
                mHooks.syncAnalyticsContextHints(oController);
                setReadinessState(oController, "ready", true, sReadyAt, "");
                ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.ANALYTICS_READY, {
                    reason: sResolvedReason,
                    source: sSelectedSource
                });
                return oResult;
            }).catch(function (oError) {
                var sErrorMessage = String((oError && oError.message) || MESSAGES.ANALYTICS_LOAD_FAILED);
                ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
                setReadinessState(oController, "error", false, "", sErrorMessage);
                throw oError;
            }).finally(function () {
                setControllerBusy(oController, false);
            });
        }
    };
});
