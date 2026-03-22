sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsViewStateReader",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/YearValue",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts"
], function (
    ControllerViewStateRuntime,
    ModelStateRuntime,
    StatePaths,
    AnalyticsContracts,
    AnalyticsUiContracts,
    ReadinessTelemetryContracts,
    ReadinessTelemetryRuntime,
    ModelContracts,
    AnalyticsViewStateReader,
    YearValue,
    AnalyticsStateConstants
) {
    "use strict";

    var LOAD_REASONS = AnalyticsUiContracts.LOAD_REASONS;
    var MESSAGES = AnalyticsUiContracts.MESSAGES;
    var PATHS = AnalyticsUiContracts.PATHS;
    var STATE_MODEL = ModelContracts.MODELS.STATE;

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

    function normalizeLoadInput(oController, sReason) {
        var oSelectionState = AnalyticsViewStateReader.readSelectionState(oController);
        return {
            selectionState: oSelectionState,
            selectedYear: YearValue.parseYearOrNull(oSelectionState.selectedYear),
            compareYear: YearValue.parseYearOrNull(oSelectionState.compareYear),
            resolvedReason: String(sReason || LOAD_REASONS.MANUAL || "manual")
        };
    }

    function validateLoadInput(oController, oLoadInput) {
        if (oLoadInput.selectedYear !== null) {
            return true;
        }
        ControllerViewStateRuntime.set(oController, PATHS.ERROR, MESSAGES.INVALID_YEAR);
        setReadinessState(oController, AnalyticsStateConstants.LOAD_STATUS.ERROR, false, "", MESSAGES.INVALID_YEAR);
        return false;
    }

    function buildLoadPayload(oLoadInput) {
        return {
            reason: oLoadInput.resolvedReason,
            selectedYear: oLoadInput.selectedYear,
            compareYear: oLoadInput.compareYear,
            selectedSource: oLoadInput.selectionState.selectedSource
        };
    }

    function syncAnalyticsSelectionState(oController, mHooks, oAnalytics) {
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
    }

    function finalizeAnalyticsLoad(oController, mHooks) {
        ControllerViewStateRuntime.set(oController, PATHS.AVAILABLE_YEARS, mHooks.buildYearOptions(oController));
        ControllerViewStateRuntime.set(oController, PATHS.COMPARE_YEAR_OPTIONS, mHooks.buildCompareYearOptions(oController));
        mHooks.setCompareYearValidation(oController, AnalyticsUiContracts.VALIDATION_STATES.NONE, "");
        mHooks.applyComparisonMetricSelection(oController);
        mHooks.applyBuilderSelection(oController);
        mHooks.syncAnalyticsContextHints(oController);
    }

    function applySuccessfulLoad(oController, oLoadInput, mHooks, oResult) {
        var oAnalytics = AnalyticsViewStateReader.readAnalyticsRoot(oController);
        var sReadyAt = new Date().toISOString();
        syncAnalyticsSelectionState(oController, mHooks, oAnalytics);
        finalizeAnalyticsLoad(oController, mHooks);
        setReadinessState(oController, AnalyticsStateConstants.LOAD_STATUS.READY, true, sReadyAt, "");
        ReadinessTelemetryRuntime.markControllerStage(oController, ReadinessTelemetryContracts.STAGES.ANALYTICS_READY, {
            reason: oLoadInput.resolvedReason,
            source: oLoadInput.selectionState.selectedSource
        });
        return oResult;
    }

    function handleLoadFailure(oController, oError) {
        var sErrorMessage = String((oError && oError.message) || MESSAGES.ANALYTICS_LOAD_FAILED);
        ControllerViewStateRuntime.set(oController, PATHS.ERROR, sErrorMessage);
        setReadinessState(oController, AnalyticsStateConstants.LOAD_STATUS.ERROR, false, "", sErrorMessage);
        throw oError;
    }

    function executeAnalyticsLoad(oController, oLoadInput, mHooks) {
        var oFacade = oController && oController._facade;
        if (!oController || !oFacade || typeof oFacade.load !== "function" || typeof oController.executeFacadeMethod !== "function") {
            return Promise.reject(new Error("ANALYTICS_LOAD_UNAVAILABLE"));
        }
        return Promise.resolve(oController.executeFacadeMethod(
            oFacade,
            "load",
            buildLoadPayload(oLoadInput),
            mHooks.buildCtx(oController)
        ));
    }

    return {
        loadAnalytics: function (oController, sReason, mHooks) {
            var oLoadInput = normalizeLoadInput(oController, sReason);

            if (!validateLoadInput(oController, oLoadInput)) {
                return Promise.resolve(false);
            }

            setReadinessState(oController, AnalyticsStateConstants.LOAD_STATUS.LOADING, false, "", "");
            setControllerBusy(oController, true, "");

            return executeAnalyticsLoad(oController, oLoadInput, mHooks).then(function (oResult) {
                return applySuccessfulLoad(oController, oLoadInput, mHooks, oResult);
            }).catch(function (oError) {
                return handleLoadFailure(oController, oError);
            }).finally(function () {
                setControllerBusy(oController, false);
            });
        }
    };
});
