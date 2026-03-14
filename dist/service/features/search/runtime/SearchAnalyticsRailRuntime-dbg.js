sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService"
], function (ControllerViewStateRuntime, ControllerModelRuntime, SchedulingRuntime, ControlStyleRuntime, TimeConfigService) {
    "use strict";

    var SEARCH_INITIAL_ANALYTICS_DELAY_MS = 400;

    function clearAnalyticsRefreshTimer(oController) {
        oController._iAnalyticsRefreshTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRefreshTimer);
    }

    function clearInitialAnalyticsSchedule(oController) {
        oController._iInitialAnalyticsTimer = SchedulingRuntime.clearTimer(oController._iInitialAnalyticsTimer);
        if (oController._iInitialAnalyticsIdleId && window.cancelIdleCallback) {
            window.cancelIdleCallback(oController._iInitialAnalyticsIdleId);
            oController._iInitialAnalyticsIdleId = null;
        }
    }

    function resolveAnalyticsRefreshMs(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
        var iMs = Number(TimeConfigService.read(oStateModel, "analyticsRefreshMs"));
        return Number.isFinite(iMs) && iMs >= 1000 ? iMs : 300000;
    }

    function pulseAnalyticsRailUpdate(oController) {
        var oRail = oController.byId("searchAnalyticsRail");
        if (!oRail) {
            return;
        }
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRailPulseTimer);
        ControlStyleRuntime.restart(oRail, "searchAnalyticsRailPulse");
        oController._iAnalyticsRailPulseTimer = SchedulingRuntime.restartTimer(0, function () {
            ControlStyleRuntime.disable(oRail, "searchAnalyticsRailPulse");
            oController._iAnalyticsRailPulseTimer = null;
        }, 520);
    }

    function refreshAnalyticsRail(oController, mOptions) {
        var bSilent = !!(mOptions && mOptions.silent);
        var fnAnalytics = mOptions && mOptions.runAnalytics;
        if (!bSilent) {
            ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", true);
            ControllerViewStateRuntime.set(oController, "/analyticsError", "");
        }
        if (typeof fnAnalytics !== "function") {
            if (!bSilent) {
                ControllerViewStateRuntime.set(oController, "/analyticsRailBusy", false);
            }
            return Promise.resolve(false);
        }
        return Promise.resolve(fnAnalytics({ intent: "refreshRail", silent: bSilent })).then(function (vResult) {
            if (bSilent) {
                pulseAnalyticsRailUpdate(oController);
            }
            return vResult;
        });
    }

    function scheduleAnalyticsRefresh(oController) {
        clearAnalyticsRefreshTimer(oController);
        oController._iAnalyticsRefreshTimer = SchedulingRuntime.restartTimer(0, function () {
            refreshAnalyticsRail(oController, {
                silent: true,
                runAnalytics: oController && oController._runSearchAnalytics
            });
            scheduleAnalyticsRefresh(oController);
        }, resolveAnalyticsRefreshMs(oController));
    }

    function bindAnalyticsRefreshTimer(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
        if (!oStateModel || oController._oAnalyticsRefreshBinding) {
            return;
        }
        if (!oController._fnAnalyticsRefreshChanged) {
            oController._fnAnalyticsRefreshChanged = function () {
                scheduleAnalyticsRefresh(oController);
            };
        }
        oController._oAnalyticsRefreshBinding = oStateModel.bindProperty("/timers/analyticsRefreshMs");
        oController._oAnalyticsRefreshBinding.attachChange(oController._fnAnalyticsRefreshChanged);
    }

    function scheduleInitialAnalytics(oController, fnBeforeRefresh) {
        clearInitialAnalyticsSchedule(oController);
        return function () {
            var fnStartAnalytics = function () {
                oController._iInitialAnalyticsIdleId = null;
                oController._iInitialAnalyticsTimer = null;
                if (typeof fnBeforeRefresh === "function") {
                    fnBeforeRefresh();
                }
                refreshAnalyticsRail(oController, {
                    silent: false,
                    runAnalytics: oController && oController._runSearchAnalytics
                });
                scheduleAnalyticsRefresh(oController);
            };
            if (window.requestIdleCallback) {
                oController._iInitialAnalyticsIdleId = window.requestIdleCallback(fnStartAnalytics, { timeout: 800 });
                return;
            }
            oController._iInitialAnalyticsTimer = SchedulingRuntime.restartTimer(0, fnStartAnalytics, SEARCH_INITIAL_ANALYTICS_DELAY_MS);
        };
    }

    return {
        bindAnalyticsRefreshTimer: bindAnalyticsRefreshTimer,
        clearAnalyticsRefreshTimer: clearAnalyticsRefreshTimer,
        clearInitialAnalyticsSchedule: clearInitialAnalyticsSchedule,
        refreshAnalyticsRail: refreshAnalyticsRail,
        scheduleAnalyticsRefresh: scheduleAnalyticsRefresh,
        scheduleInitialAnalytics: scheduleInitialAnalytics
    };
});
