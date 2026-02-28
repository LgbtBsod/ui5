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
        return runSmartSearch(mArgs);
    }

    return {
        runSmartSearch: runSmartSearch,
        runSearchTrigger: runSearchTrigger
    };
});
