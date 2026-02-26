sap.ui.define([], function () {
    "use strict";

    var TRIGGERS = {
        ROUTE_MATCHED: true,
        SMART_SEARCH: true,
        FALLBACK_SEARCH: true,
        RETRY_LOAD: true,
        RESET_FILTERS: true,
        SEARCH_MODE_TOGGLE: true
    };

    function shouldRefreshForTrigger(sTrigger) {
        return !!TRIGGERS[String(sTrigger || "")];
    }

    function ensureRefreshState(oState) {
        var oValue = oState || {};
        if (typeof oValue.latestRequestId !== "number") {
            oValue.latestRequestId = 0;
        }
        return oValue;
    }

    function runRefreshLifecycle(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        var fnLoad = mArgs && mArgs.loadAnalytics;
        var fnApply = mArgs && mArgs.applyPresentation;
        var oState = ensureRefreshState(mArgs && mArgs.refreshState);

        if (!oViewModel || typeof oViewModel.setProperty !== "function" || typeof fnLoad !== "function") {
            return Promise.resolve({ applied: false, reason: "missing_adapter" });
        }

        var iRequestId = ++oState.latestRequestId;

        return Promise.resolve().then(function () {
            return fnLoad();
        }).then(function (oAnalytics) {
            if (iRequestId !== oState.latestRequestId) {
                return { applied: false, stale: true, analytics: oAnalytics || null };
            }

            if (typeof fnApply === "function") {
                fnApply(oAnalytics || oViewModel.getProperty("/analytics") || {});
            }

            return { applied: true, stale: false, analytics: oAnalytics || null };
        }).catch(function (oError) {
            return {
                applied: false,
                stale: false,
                error: (oError && oError.message) || "refresh_failed"
            };
        });
    }

    return {
        shouldRefreshForTrigger: shouldRefreshForTrigger,
        ensureRefreshState: ensureRefreshState,
        runRefreshLifecycle: runRefreshLifecycle
    };
});
