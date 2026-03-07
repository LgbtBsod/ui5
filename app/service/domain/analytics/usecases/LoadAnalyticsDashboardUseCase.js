sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/analytics/AnalyticsPayloadNormalizer"
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
            selectedYear: Number(mInput && mInput.selectedYear) || 0
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
