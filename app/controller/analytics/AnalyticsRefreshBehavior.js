sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsUiContracts",
    "PRODUCTION_CONTROL_CHECKLIST/controller/analytics/AnalyticsRefreshRuntime"
], function (ControllerViewStateRuntime, AnalyticsContracts, AnalyticsUiContracts, AnalyticsRefreshRuntime) {
    "use strict";

    var MESSAGES = AnalyticsUiContracts.MESSAGES;
    var PATHS = AnalyticsUiContracts.PATHS;

    function onRefreshAnalytics(oController, fnBuildCtx, fnPollRefreshStateUntilSettled, fnLoadAnalytics) {
        var oCtx = fnBuildCtx(oController);
        var oRefreshState = ControllerViewStateRuntime.get(oController, PATHS.REFRESH_STATE, {}) || {};
        if (AnalyticsRefreshRuntime.isRefreshQueued(oRefreshState)) {
            ControllerViewStateRuntime.set(oController, PATHS.REFRESH_BUSY, true);
            return fnPollRefreshStateUntilSettled(oController, AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS).then(function () {
                return fnLoadAnalytics(oController, "pollRefresh");
            }).finally(function () {
                ControllerViewStateRuntime.set(oController, PATHS.REFRESH_BUSY, false);
            });
        }
        AnalyticsRefreshRuntime.invalidatePolls(oController);
        ControllerViewStateRuntime.setMany(oController, {
            [PATHS.REFRESH_BUSY]: true,
            [PATHS.ERROR]: ""
        });
        return Promise.resolve(oCtx && oCtx.analytics && oCtx.analytics.requestRefresh ? oCtx.analytics.requestRefresh({
            requestedBy: AnalyticsContracts.REFRESH.REQUESTED_BY_WEB
        }) : null).then(function (oState) {
            if (oState) {
                ControllerViewStateRuntime.set(oController, PATHS.REFRESH_STATE, oState);
            }
            return fnPollRefreshStateUntilSettled(oController, AnalyticsRefreshRuntime.REFRESH_POLL_MAX_ATTEMPTS);
        }).then(function () {
            return fnLoadAnalytics(oController, "manualRefresh");
        }).catch(function (oError) {
            ControllerViewStateRuntime.set(oController, PATHS.ERROR, String((oError && oError.message) || MESSAGES.ANALYTICS_REFRESH_FAILED));
            throw oError;
        }).finally(function () {
            ControllerViewStateRuntime.set(oController, PATHS.REFRESH_BUSY, false);
        });
    }

    return {
        onRefreshAnalytics: onRefreshAnalytics
    };
});
