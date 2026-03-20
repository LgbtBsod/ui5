sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsPayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsUiConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/YearValue",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsStateConstants"
], function (Result, Effects, AnalyticsPayloadNormalizer, StatePaths, WorkflowTelemetry, AnalyticsUiContracts, YearValue, AnalyticsStateConstants) {
    "use strict";

    var TELEMETRY_EVENTS = Object.freeze({
        ERROR: "analytics.dashboard.error",
        LOADED: "analytics.dashboard.loaded",
        STALE: "analytics.dashboard.stale"
    });
    var MODEL_NAMES = Object.freeze({
        STATE: "state",
        VIEW: "view"
    });
    var VIEW_PATHS = Object.freeze({
        ANALYTICS: AnalyticsUiContracts.PATHS.ANALYTICS,
        BUSY: "/busy",
        ERROR: AnalyticsUiContracts.PATHS.ERROR
    });
    var FALLBACK_MESSAGES = Object.freeze({
        ERROR: "Analytics unavailable",
        INTERNAL_ERROR: "analytics_unavailable"
    });

    function buildNormalizedRequest(mInput) {
        return {
            selectedYear: YearValue.parseYearOrNull(mInput && mInput.selectedYear),
            compareYear: YearValue.parseYearOrNull(mInput && mInput.compareYear),
            selectedSource: YearValue.toTrimmedString(mInput && mInput.selectedSource).toUpperCase()
        };
    }

    function resolveErrorMessage(oError) {
        return String((oError && oError.message) || FALLBACK_MESSAGES.INTERNAL_ERROR);
    }

    function buildReadyEffects(oDashboard, sReadyAt) {
        return [
            Effects.modelPatch(MODEL_NAMES.STATE, StatePaths.UI_BUSY_ANALYTICS, false),
            Effects.modelPatch(MODEL_NAMES.STATE, StatePaths.READINESS_ANALYTICS, {
                status: AnalyticsStateConstants.LOAD_STATUS.READY,
                ready: true,
                readyAt: sReadyAt,
                error: ""
            }),
            Effects.modelPatch(MODEL_NAMES.VIEW, VIEW_PATHS.BUSY, false),
            Effects.modelPatch(MODEL_NAMES.VIEW, VIEW_PATHS.ERROR, ""),
            Effects.modelPatch(MODEL_NAMES.VIEW, VIEW_PATHS.ANALYTICS, oDashboard)
        ];
    }

    function buildErrorEffects(sErrorMessage) {
        return [
            Effects.modelPatch(MODEL_NAMES.STATE, StatePaths.UI_BUSY_ANALYTICS, false),
            Effects.modelPatch(MODEL_NAMES.STATE, StatePaths.READINESS_ANALYTICS, {
                status: AnalyticsStateConstants.LOAD_STATUS.ERROR,
                ready: false,
                readyAt: "",
                error: sErrorMessage
            }),
            Effects.modelPatch(MODEL_NAMES.VIEW, VIEW_PATHS.BUSY, false),
            Effects.modelPatch(MODEL_NAMES.VIEW, VIEW_PATHS.ERROR, sErrorMessage || FALLBACK_MESSAGES.ERROR)
        ];
    }

    function LoadAnalyticsDashboardUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var oAnalytics = mCtx && mCtx.analytics;
        var mRequest = buildNormalizedRequest(mInput);
        var sReadyAt = new Date().toISOString();
        var pDetailed = (oAnalytics && typeof oAnalytics.fetchDetailed === "function")
            ? oAnalytics.fetchDetailed(mRequest)
            : Promise.resolve(AnalyticsPayloadNormalizer.createEmptyDashboard());

        return Promise.resolve(pDetailed).then(function (oSummary) {
            var oDashboard = AnalyticsPayloadNormalizer.normalizeDashboard(oSummary);
            WorkflowTelemetry.emit(TELEMETRY_EVENTS.LOADED, {
                stateModel: mCtx && mCtx.stateModel,
                payload: {
                    selectedYear: mRequest.selectedYear,
                    compareYear: mRequest.compareYear,
                    selectedSource: mRequest.selectedSource,
                    readyAt: sReadyAt
                }
            });

            return Result.ok({ analytics: oDashboard }, buildReadyEffects(oDashboard, sReadyAt));
        }).catch(function (oError) {
            var sErrorMessage = resolveErrorMessage(oError);
            if (String((oError && oError.code) || "").trim().toUpperCase() === "OUTDATED_RESPONSE") {
                WorkflowTelemetry.emit(TELEMETRY_EVENTS.STALE, {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: {
                        selectedYear: mRequest.selectedYear,
                        compareYear: mRequest.compareYear,
                        selectedSource: mRequest.selectedSource
                    }
                });
                return Result.ok({ ignored: true }, []);
            }
            WorkflowTelemetry.emit(TELEMETRY_EVENTS.ERROR, {
                stateModel: mCtx && mCtx.stateModel,
                payload: {
                    selectedYear: mRequest.selectedYear,
                    compareYear: mRequest.compareYear,
                    selectedSource: mRequest.selectedSource,
                    error: sErrorMessage
                }
            });
            return Result.fail(oError, buildErrorEffects(sErrorMessage));
        });
    }

    return LoadAnalyticsDashboardUseCase;
});