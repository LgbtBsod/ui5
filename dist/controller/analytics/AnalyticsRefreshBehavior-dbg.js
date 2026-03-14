sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime"
], function (ControllerViewStateRuntime, AnalyticsContracts, AnalyticsRefreshRuntime) {
    "use strict";

    function onRefreshAnalytics(oController, fnBuildCtx, fnPollRefreshStateUntilSettled, fnLoadAnalytics) {
        var oCtx = fnBuildCtx(oController);
        var oRefreshState = ControllerViewStateRuntime.get(oController, "/refreshState", {}) || {};
        if (AnalyticsRefreshRuntime.isRefreshQueued(oRefreshState)) {
            ControllerViewStateRuntime.set(oController, "/refreshBusy", true);
            return fnPollRefreshStateUntilSettled(oController, AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS).then(function () {
                return fnLoadAnalytics(oController, "pollRefresh");
            }).then(function (oResult) {
                ControllerViewStateRuntime.set(oController, "/refreshBusy", false);
                return oResult;
            }, function (oError) {
                ControllerViewStateRuntime.set(oController, "/refreshBusy", false);
                throw oError;
            });
        }
        ControllerViewStateRuntime.setMany(oController, {
            "/refreshBusy": true,
            "/error": ""
        });
        return Promise.resolve(oCtx && oCtx.analytics && oCtx.analytics.requestRefresh ? oCtx.analytics.requestRefresh({
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
        }).then(function (oResult) {
            ControllerViewStateRuntime.set(oController, "/refreshBusy", false);
            return oResult;
        }, function (oError) {
            ControllerViewStateRuntime.set(oController, "/refreshBusy", false);
            throw oError;
        });
    }

    return {
        onRefreshAnalytics: onRefreshAnalytics
    };
});
