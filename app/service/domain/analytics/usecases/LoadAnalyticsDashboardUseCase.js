sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/framework/Effects",
    "sap_ui5/service/domain/analytics/AnalyticsPayloadNormalizer"
], function (UseCase, Result, Effects, AnalyticsPayloadNormalizer) {
    "use strict";

    function LoadAnalyticsDashboardUseCase() {
        UseCase.call(this, "LoadAnalyticsDashboardUseCase");
    }

    LoadAnalyticsDashboardUseCase.prototype = Object.create(UseCase.prototype);
    LoadAnalyticsDashboardUseCase.prototype.constructor = LoadAnalyticsDashboardUseCase;

    LoadAnalyticsDashboardUseCase.prototype.execute = function (_mInput, mCtx) {
        var oAnalytics = mCtx && mCtx.analytics;
        var pDetailed = (oAnalytics && typeof oAnalytics.fetchDetailed === "function")
            ? oAnalytics.fetchDetailed()
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
