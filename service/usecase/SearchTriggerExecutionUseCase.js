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

    function runFallbackSearch(mArgs) {
        return mArgs.runFallbackSearchLifecycle({
            markSearchedAndRebind: mArgs.markSearchedAndRebind,
            syncFilterHint: mArgs.syncFilterHint,
            refreshInlineAnalytics: mArgs.refreshInlineAnalytics
        });
    }

    function runSearchTrigger(mArgs) {
        if (mArgs.useSmartControls) {
            return runSmartSearch(mArgs);
        }
        return runFallbackSearch(mArgs);
    }

    return {
        runSmartSearch: runSmartSearch,
        runFallbackSearch: runFallbackSearch,
        runSearchTrigger: runSearchTrigger
    };
});
