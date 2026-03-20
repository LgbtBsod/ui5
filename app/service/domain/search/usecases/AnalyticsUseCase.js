sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsPayloadNormalizer",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry"
], function (Result, Effects, AnalyticsPayloadNormalizer, WorkflowTelemetry) {
    "use strict";

    function AnalyticsUseCase() {
        return {
            execute: execute
        };
    }

function execute(mInput, mCtx) {
        var sIntent = (mInput && mInput.intent) || "";
        var bSilent = !!(mInput && mInput.silent);

        if (!sIntent) {
            return Promise.resolve(Result.ok({}, []));
        }
        var oAnalytics = mCtx && mCtx.analytics;

        if (sIntent === "refreshRail") {
            var pSummary = (oAnalytics && typeof oAnalytics.fetchSummary === "function")
                ? oAnalytics.fetchSummary()
                : Promise.resolve(AnalyticsPayloadNormalizer.buildRailPayload({}));

            return Promise.resolve(pSummary).then(function (oSummary) {
                WorkflowTelemetry.emit("analytics.search.loaded", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: {
                        silent: bSilent,
                        refreshedAt: new Date().toISOString()
                    }
                });
                var aEffects = [
                    Effects.modelPatch("view", "/analyticsError", ""),
                    Effects.modelPatch("view", "/analyticsRail", AnalyticsPayloadNormalizer.buildRailPayload(oSummary))
                ];

                if (!bSilent) {
                    aEffects.unshift(Effects.modelPatch("view", "/analyticsRailBusy", false));
                }
                return Result.ok({ analyticsRail: oSummary || {} }, aEffects);
            }).catch(function (oError) {
                if (String((oError && oError.code) || "").trim().toUpperCase() === "OUTDATED_RESPONSE") {
                    WorkflowTelemetry.emit("analytics.search.stale", {
                        stateModel: mCtx && mCtx.stateModel,
                        payload: {
                            silent: bSilent
                        }
                    });
                    return Result.ok({ ignored: true }, []);
                }
                WorkflowTelemetry.emit("analytics.search.error", {
                    stateModel: mCtx && mCtx.stateModel,
                    payload: {
                        silent: bSilent,
                        error: String((oError && oError.message) || "analytics_unavailable")
                    }
                });
                var aEffects = [
                    Effects.modelPatch("view", "/analyticsError", String((oError && oError.message) || "Analytics unavailable"))
                ];

                if (!bSilent) {
                    aEffects.unshift(Effects.modelPatch("view", "/analyticsRailBusy", false));
                }
                return Result.fail(oError, aEffects);
            });
        }

        return Promise.resolve(Result.ok({}, []));
    }

    return AnalyticsUseCase;
});