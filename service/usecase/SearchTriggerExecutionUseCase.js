sap.ui.define([], function () {
    "use strict";

    function runSmartSearch(mArgs) {
        mArgs.syncStateFilters();
        mArgs.markSearchedAndRebind();
        mArgs.runTriggerPolicy({
            trigger: "SMART_SEARCH",
            syncFilterHint: mArgs.syncFilterHint,
            refreshInlineAnalytics: mArgs.refreshInlineAnalytics
        });
        return { ok: true, mode: "smart" };
    }

    function runSearchTrigger(mArgs) {
        if (!mArgs || mArgs.useSmartControls === false) {
            if (mArgs && typeof mArgs.runFallbackSearchLifecycle === "function") {
                return mArgs.runFallbackSearchLifecycle();
            }
            return { ok: false, reason: "smart_controls_disabled" };
        }
        return runSmartSearch(mArgs);
    }

    return {
        runSmartSearch: runSmartSearch,
        runSearchTrigger: runSearchTrigger
    };
});
