sap.ui.define([], function () {
    "use strict";

    function markSearchedAndRebind(oViewModel, fnRebind) {
        oViewModel.setProperty("/hasSearched", true);
        fnRebind();
    }

    function applyStatusFilter(oStateModel, sFilterPath, sFilterValue, fnRebind) {
        if (!sFilterPath) {
            return false;
        }
        oStateModel.setProperty(sFilterPath, sFilterValue);
        fnRebind();
        return true;
    }

    return {
        markSearchedAndRebind: markSearchedAndRebind,
        applyStatusFilter: applyStatusFilter
    };
});
