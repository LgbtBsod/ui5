sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/PromiseRuntime"
], function (ControllerViewStateRuntime, AnalyticsContracts, AnalyticsRefreshRuntime, PromiseRuntime) {
    "use strict";

    function onRefreshAnalytics(oController, fnBuildCtx, fnPollRefreshStateUntilSettled, fnLoadAnalytics) {
        var oCtx = fnBuildCtx(oController);
        var oRefreshState = ControllerViewStateRuntime.get(oController, "/refreshState", {}) || {};
        if (AnalyticsRefreshRuntime.isRefreshQueued(oRefreshState)) {
            ControllerViewStateRuntime.set(oController, "/refreshBusy", true);
            return PromiseRuntime.withFinally(fnPollRefreshStateUntilSettled(oController, AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS).then(function () {
                return fnLoadAnalytics(oController, "pollRefresh");
            }), function () {
                ControllerViewStateRuntime.set(oController, "/refreshBusy", false);
            });
        }
        AnalyticsRefreshRuntime.invalidatePolls(oController);
        ControllerViewStateRuntime.setMany(oController, {
            "/refreshBusy": true,
            "/error": ""
        });
        return PromiseRuntime.withFinally(Promise.resolve(oCtx && oCtx.analytics && oCtx.analytics.requestRefresh ? oCtx.analytics.requestRefresh({
            requestedBy: AnalyticsContracts.REFRESH.REQUESTED_BY_WEB
        }) : null).then(function (oState) {
            if (oState) {
                ControllerViewStateRuntime.set(oController, "/refreshState", oState);
            }
            return fnPollRefreshStateUntilSettled(oController, AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS);
        }).then(function () {
            return fnLoadAnalytics(oController, "manualRefresh");
        }).catch(function (oError) {
            ControllerViewStateRuntime.set(oController, "/error", String((oError && oError.message) || "Analytics refresh failed"));
            throw oError;
        }), function () {
            ControllerViewStateRuntime.set(oController, "/refreshBusy", false);
        });
    }

    return {
        onRefreshAnalytics: onRefreshAnalytics
    };
});
