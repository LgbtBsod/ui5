sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/framework/Effects",
    "checklist/app/service/domain/analytics/AnalyticsPayloadNormalizer"
], function (UseCase, Result, Effects, AnalyticsPayloadNormalizer) {
    "use strict";

    function AnalyticsUseCase() {
        UseCase.call(this, "AnalyticsUseCase");
    }

    AnalyticsUseCase.prototype = Object.create(UseCase.prototype);
    AnalyticsUseCase.prototype.constructor = AnalyticsUseCase;

    AnalyticsUseCase.prototype.execute = function (mInput, mCtx) {
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
                var aEffects = [
                    Effects.modelPatch("view", "/analyticsError", ""),
                    Effects.modelPatch("view", "/analyticsRail", AnalyticsPayloadNormalizer.buildRailPayload(oSummary))
                ];

                if (!bSilent) {
                    aEffects.unshift(Effects.modelPatch("view", "/analyticsRailBusy", false));
                }
                return Result.ok({ analyticsRail: oSummary || {} }, aEffects);
            }).catch(function (oError) {
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
    };

    return AnalyticsUseCase;
});
