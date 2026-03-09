sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsPayloadNormalizer"
], function (UseCase, Result, Effects, AnalyticsPayloadNormalizer) {
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
        var pDetailed = (oAnalytics && typeof oAnalytics.fetchDetailed === "function")
            ? oAnalytics.fetchDetailed(mRequest)
            : Promise.resolve(AnalyticsPayloadNormalizer.createEmptyDashboard());

        return Promise.resolve(pDetailed).then(function (oSummary) {
            var oDashboard = AnalyticsPayloadNormalizer.normalizeDashboard(oSummary);

            return Result.ok({ analytics: oDashboard }, [
                Effects.modelPatch("view", "/busy", false),
                Effects.modelPatch("view", "/error", ""),
                Effects.modelPatch("view", "/analytics", oDashboard)
            ]);
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("view", "/busy", false),
                Effects.modelPatch("view", "/error", String((oError && oError.message) || "Analytics unavailable"))
            ]);
        });
    };

    return LoadAnalyticsDashboardUseCase;
});
