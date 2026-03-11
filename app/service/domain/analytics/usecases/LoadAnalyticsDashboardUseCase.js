sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsPayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (UseCase, Result, Effects, AnalyticsPayloadNormalizer, StatePaths) {
    "use strict";

    function LoadAnalyticsDashboardUseCase() {
        UseCase.call(this, "LoadAnalyticsDashboardUseCase");
    }

    LoadAnalyticsDashboardUseCase.prototype = Object.create(UseCase.prototype);
    LoadAnalyticsDashboardUseCase.prototype.constructor = LoadAnalyticsDashboardUseCase;

    LoadAnalyticsDashboardUseCase.prototype.execute = function (mInput, mCtx) {
        var oAnalytics = mCtx && mCtx.analytics;
        var mRequest = {
            selectedYear: Number(mInput && mInput.selectedYear) || 0,
            compareYear: Number(mInput && mInput.compareYear) || 0,
            selectedSource: String(mInput && mInput.selectedSource || "").trim()
        };
        var sReadyAt = new Date().toISOString();
        var pDetailed = (oAnalytics && typeof oAnalytics.fetchDetailed === "function")
            ? oAnalytics.fetchDetailed(mRequest)
            : Promise.resolve(AnalyticsPayloadNormalizer.createEmptyDashboard());

        return Promise.resolve(pDetailed).then(function (oSummary) {
            var oDashboard = AnalyticsPayloadNormalizer.normalizeDashboard(oSummary);

            return Result.ok({ analytics: oDashboard }, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_ANALYTICS, false),
                Effects.modelPatch("state", StatePaths.READINESS_ANALYTICS, {
                    status: "ready",
                    ready: true,
                    readyAt: sReadyAt,
                    error: ""
                }),
                Effects.modelPatch("view", "/busy", false),
                Effects.modelPatch("view", "/error", ""),
                Effects.modelPatch("view", "/analytics", oDashboard)
            ]);
        }).catch(function (oError) {
            if (String((oError && oError.code) || "").trim().toUpperCase() === "OUTDATED_RESPONSE") {
                return Result.ok({ ignored: true }, []);
            }
            return Result.fail(oError, [
                Effects.modelPatch("state", StatePaths.UI_BUSY_ANALYTICS, false),
                Effects.modelPatch("state", StatePaths.READINESS_ANALYTICS, {
                    status: "error",
                    ready: false,
                    readyAt: "",
                    error: String((oError && oError.message) || "analytics_unavailable")
                }),
                Effects.modelPatch("view", "/busy", false),
                Effects.modelPatch("view", "/error", String((oError && oError.message) || "Analytics unavailable"))
            ]);
        });
    };

    return LoadAnalyticsDashboardUseCase;
});
