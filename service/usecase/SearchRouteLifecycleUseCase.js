sap.ui.define([], function () {
    "use strict";

    function runRouteMatchedLifecycle(mArgs) {
        var fnApplyDefaults = mArgs && mArgs.applyDefaults;
        if (typeof fnApplyDefaults !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_state_model" });
        }

        fnApplyDefaults();

        var fnSyncSmartControls = mArgs && mArgs.syncSmartControls;
        if (typeof fnSyncSmartControls === "function") {
            fnSyncSmartControls();
        }

        var fnSyncSelection = mArgs && mArgs.syncSelection;
        if (typeof fnSyncSelection !== "function") {
            return Promise.resolve({ ok: false, reason: "missing_selection_adapter" });
        }

        var fnUpdateSummary = mArgs && mArgs.updateSummary;
        var fnApplyEmptyState = mArgs && mArgs.applyEmptyState;
        var fnRefreshAnalytics = mArgs && mArgs.refreshAnalytics;

        return Promise.resolve(fnSyncSelection()).then(function () {
            if (typeof fnUpdateSummary === "function") {
                fnUpdateSummary();
            }
            if (typeof fnRefreshAnalytics === "function") {
                fnRefreshAnalytics("ROUTE_MATCHED");
            }
            if (typeof fnApplyEmptyState === "function") {
                fnApplyEmptyState();
            }
            return { ok: true, reason: "route_matched_applied" };
        });
    }

    return {
        runRouteMatchedLifecycle: runRouteMatchedLifecycle
    };
});
