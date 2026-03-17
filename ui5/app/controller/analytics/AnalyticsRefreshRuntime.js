sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/AnalyticsContracts"
], function (ControllerViewStateRuntime, SchedulingRuntime, AnalyticsContracts) {
    "use strict";

    var REFRESH_POLL_DELAY_MS = AnalyticsContracts.REFRESH.POLL_DELAY_MS;
    var REFRESH_POLL_MAX_ATTEMPTS = AnalyticsContracts.REFRESH.POLL_MAX_ATTEMPTS;

    function isRefreshQueued(oRefreshState) {
        var sStatus = String(oRefreshState && oRefreshState.status || "").trim().toUpperCase();
        return !!(oRefreshState && oRefreshState.isRunning) ||
            sStatus === AnalyticsContracts.REFRESH.STATUSES.REQUESTED ||
            sStatus === AnalyticsContracts.REFRESH.STATUSES.RUNNING;
    }

    function pollRefreshStateUntilSettled(oController, iAttemptsLeft, fnFetchRefreshState) {
        var iRemaining = Number(iAttemptsLeft);
        var iPollToken = Number(oController && oController._iAnalyticsRefreshPollToken || 0);
        if (!isFinite(iRemaining) || iRemaining <= 0) {
            return Promise.resolve({});
        }
        return Promise.resolve(fnFetchRefreshState()).then(function (oState) {
            var oRefreshState = oState || {};
            var sStatus = String(oRefreshState.status || "").toUpperCase();
            var bActive = !!oRefreshState.isRunning ||
                sStatus === AnalyticsContracts.REFRESH.STATUSES.REQUESTED ||
                sStatus === AnalyticsContracts.REFRESH.STATUSES.RUNNING;
            if (Number(oController && oController._iAnalyticsRefreshPollToken || 0) !== iPollToken) {
                return oRefreshState;
            }
            ControllerViewStateRuntime.set(oController, "/refreshState", oRefreshState);
            if (!bActive || iRemaining <= 0) {
                return oRefreshState;
            }
            return SchedulingRuntime.wait(REFRESH_POLL_DELAY_MS).then(function () {
                if (Number(oController && oController._iAnalyticsRefreshPollToken || 0) !== iPollToken) {
                    return oRefreshState;
                }
                return pollRefreshStateUntilSettled(oController, iRemaining - 1, fnFetchRefreshState);
            });
        });
    }

    return {
        isRefreshQueued: isRefreshQueued,
        pollRefreshStateUntilSettled: pollRefreshStateUntilSettled,
        invalidatePolls: function (oController) {
            if (oController) {
                oController._iAnalyticsRefreshPollToken = Number(oController._iAnalyticsRefreshPollToken || 0) + 1;
            }
        },
        REFRESH_POLL_MAX_ATTEMPTS: REFRESH_POLL_MAX_ATTEMPTS
    };
});
