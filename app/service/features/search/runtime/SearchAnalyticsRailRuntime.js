sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime"
], function (ControllerViewStateRuntime, SchedulingRuntime, ControlStyleRuntime) {
    "use strict";

    function clearAnalyticsRefreshTimer(oController) {
        oController._iAnalyticsRefreshTimer = SchedulingRuntime.clearTimer(oController._iAnalyticsRefreshTimer);
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

    function bindAnalyticsRefreshTimer(oController) {
        clearAnalyticsRefreshTimer(oController);
    }

    return {
        bindAnalyticsRefreshTimer: bindAnalyticsRefreshTimer,
        clearAnalyticsRefreshTimer: clearAnalyticsRefreshTimer,
        refreshAnalyticsRail: refreshAnalyticsRail
    };
});
