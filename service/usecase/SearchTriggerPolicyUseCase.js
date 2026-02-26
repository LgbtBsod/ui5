sap.ui.define([], function () {
    "use strict";

    var TRIGGERS = {
        SMART_FILTER_CHANGE: { syncFilterHint: true, refreshAnalytics: false, requiresStateModel: false },
        SMART_SEARCH: { syncFilterHint: true, refreshAnalytics: true, analyticsTrigger: "SMART_SEARCH", requiresStateModel: false },
        STATUS_FILTER_PRESS: { syncFilterHint: true, refreshAnalytics: true, analyticsTrigger: "SMART_SEARCH", requiresStateModel: false },
        RESET_FILTERS: { syncFilterHint: true, refreshAnalytics: true, analyticsTrigger: "RESET_FILTERS", requiresStateModel: false },
        SEARCH_MODE_TOGGLE: { syncFilterHint: true, refreshAnalytics: true, analyticsTrigger: "SEARCH_MODE_TOGGLE", requiresStateModel: true }
    };

    function runTriggerPolicy(mArgs) {
        var sTrigger = String((mArgs && mArgs.trigger) || "");
        var oPolicy = TRIGGERS[sTrigger];
        if (!oPolicy) {
            return { ok: false, reason: "unsupported_trigger", trigger: sTrigger };
        }

        if (oPolicy.requiresStateModel && !(mArgs && mArgs.stateModel)) {
            return { ok: false, reason: "missing_state_model", trigger: sTrigger };
        }

        var fnSyncFilterHint = mArgs && mArgs.syncFilterHint;
        if (oPolicy.syncFilterHint && typeof fnSyncFilterHint === "function") {
            fnSyncFilterHint();
        }

        if (oPolicy.refreshAnalytics) {
            var fnRefreshInlineAnalytics = mArgs && mArgs.refreshInlineAnalytics;
            if (typeof fnRefreshInlineAnalytics !== "function") {
                return { ok: false, reason: "missing_refresh_adapter", trigger: sTrigger };
            }
            fnRefreshInlineAnalytics(oPolicy.analyticsTrigger);
        }

        return {
            ok: true,
            reason: "trigger_applied",
            trigger: sTrigger,
            analyticsTrigger: oPolicy.analyticsTrigger || ""
        };
    }

    return {
        runTriggerPolicy: runTriggerPolicy
    };
});
