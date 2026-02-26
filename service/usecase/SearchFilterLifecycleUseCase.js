sap.ui.define([], function () {
    "use strict";

    function runResetLifecycle(mArgs) {
        var fnResetFilters = mArgs && mArgs.resetFilters;
        if (typeof fnResetFilters === "function") {
            fnResetFilters();
        }

        var fnClearSmartFilters = mArgs && mArgs.clearSmartFilters;
        if (typeof fnClearSmartFilters === "function") {
            fnClearSmartFilters();
        }

        var fnRebind = mArgs && mArgs.rebind;
        if (typeof fnRebind === "function") {
            fnRebind();
        }

        var fnSyncSelectionState = mArgs && mArgs.syncSelectionState;
        if (typeof fnSyncSelectionState === "function") {
            fnSyncSelectionState();
        }

        var fnRefreshInlineAnalytics = mArgs && mArgs.refreshInlineAnalytics;
        if (typeof fnRefreshInlineAnalytics === "function") {
            fnRefreshInlineAnalytics("RESET_FILTERS");
        }

        return { ok: true, reason: "reset_applied" };
    }

    function runSearchModeToggleLifecycle(mArgs) {
        var bLoose = !!(mArgs && mArgs.looseMode);

        var fnApplySearchMode = mArgs && mArgs.applySearchMode;
        if (typeof fnApplySearchMode === "function") {
            fnApplySearchMode(bLoose);
        }

        var fnUpdateFilterState = mArgs && mArgs.updateFilterState;
        if (typeof fnUpdateFilterState === "function") {
            fnUpdateFilterState();
        }

        var fnRefreshInlineAnalytics = mArgs && mArgs.refreshInlineAnalytics;
        if (typeof fnRefreshInlineAnalytics === "function") {
            fnRefreshInlineAnalytics("SEARCH_MODE_TOGGLE");
        }

        return { ok: true, reason: "search_mode_applied", looseMode: bLoose };
    }

    return {
        runResetLifecycle: runResetLifecycle,
        runSearchModeToggleLifecycle: runSearchModeToggleLifecycle
    };
});
