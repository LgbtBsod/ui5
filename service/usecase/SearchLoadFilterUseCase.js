sap.ui.define([], function () {
    "use strict";

    function resetFilters(oStateModel, oViewModel) {
        oStateModel.setProperty("/filterId", "");
        oStateModel.setProperty("/filterLpc", "");
        oStateModel.setProperty("/filterFailedChecks", "ALL");
        oStateModel.setProperty("/filterFailedBarriers", "ALL");
        oStateModel.setProperty("/search/failSegment", "ALL");
        oStateModel.setProperty("/search/barrierFailSegment", "ALL");
        oViewModel.setProperty("/hasSearched", true);
    }

    function applySearchMode(oStateModel, oViewModel, bLooseMode) {
        oStateModel.setProperty("/searchMode", bLooseMode ? "LOOSE" : "EXACT");
        oViewModel.setProperty("/hasSearched", true);
    }

    function runRetryLoad(mArgs) {
        mArgs.resetLoadError();
        return mArgs.runWithLoading(function () {
            return mArgs.getCheckLists().then(function (aCheckLists) {
                mArgs.applyLoadedRows(aCheckLists);
            }).catch(function (oError) {
                mArgs.applyLoadError(oError);
            });
        });
    }

    return {
        resetFilters: resetFilters,
        applySearchMode: applySearchMode,
        runRetryLoad: runRetryLoad
    };
});
