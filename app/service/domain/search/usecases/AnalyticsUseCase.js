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
        var bUserInitiated = !!(mInput && mInput.userInitiated);

        if (!sIntent) {
            return Promise.resolve(Result.ok({}, []));
        }
        if (sIntent === "closeDialog") {
            return Promise.resolve(Result.ok({}, [
                Effects.modelPatch("view", "/analyticsBusy", false),
                Effects.dialog("workflowAnalytics", "close", {})
            ]));
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

        if (sIntent !== "openDialog" || !bUserInitiated) {
            return Promise.resolve(Result.ok({}, [
                Effects.modelPatch("view", "/analyticsBusy", false)
            ]));
        }

        var pDetailed = (oAnalytics && typeof oAnalytics.fetchDetailed === "function")
            ? oAnalytics.fetchDetailed()
            : Promise.resolve(AnalyticsPayloadNormalizer.createEmptyDashboard());

        return Promise.resolve(pDetailed).then(function (oSummary) {
            var mAnalytics = AnalyticsPayloadNormalizer.normalizeDashboard(oSummary);

            return Result.ok({ analytics: mAnalytics }, [
                Effects.modelPatch("view", "/analyticsBusy", false),
                Effects.modelPatch("view", "/analyticsError", ""),
                Effects.modelPatch("view", "/analytics", mAnalytics)
            ]);
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("view", "/analyticsBusy", false),
                Effects.modelPatch("view", "/analyticsError", String((oError && oError.message) || "Analytics unavailable"))
            ]);
        });
    };

    return AnalyticsUseCase;
});
