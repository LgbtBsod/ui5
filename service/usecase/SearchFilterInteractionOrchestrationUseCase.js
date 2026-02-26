sap.ui.define([], function () {
    "use strict";

    function runReset(mArgs) {
        return mArgs.runResetLifecycle({
            resetFilters: mArgs.resetFilters,
            clearSmartFilters: mArgs.clearSmartFilters,
            rebind: mArgs.rebind,
            syncSelectionState: mArgs.syncSelectionState,
            refreshInlineAnalytics: function (sTrigger) {
                mArgs.runTriggerPolicy({
                    trigger: sTrigger,
                    stateModel: mArgs.stateModel,
                    syncFilterHint: mArgs.syncFilterHint,
                    refreshInlineAnalytics: mArgs.refreshInlineAnalytics
                });
            }
        });
    }

    function runSearchModeToggle(mArgs) {
        return mArgs.runSearchModeToggleLifecycle({
            looseMode: mArgs.looseMode,
            applySearchMode: mArgs.applySearchMode,
            updateFilterState: mArgs.updateFilterState,
            refreshInlineAnalytics: function (sTrigger) {
                mArgs.runTriggerPolicy({
                    trigger: sTrigger,
                    stateModel: mArgs.stateModel,
                    syncFilterHint: mArgs.syncFilterHint,
                    refreshInlineAnalytics: mArgs.refreshInlineAnalytics
                });
            }
        });
    }

    return {
        runReset: runReset,
        runSearchModeToggle: runSearchModeToggle
    };
});
