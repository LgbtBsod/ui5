sap.ui.define([], function () {
    "use strict";

    function runSaveFlow(mArgs) {
        return mArgs.saveChecklist().then(function (oSavedChecklist) {
            return mArgs.loadChecklistCollection().then(function (aUpdatedCheckLists) {
                return {
                    savedChecklist: oSavedChecklist,
                    checkLists: aUpdatedCheckLists
                };
            });
        }).then(function (oResult) {
            mArgs.applySaveResult(oResult);
            return oResult;
        }).catch(function (oError) {
            return mArgs.handleSaveError(oError);
        });
    }

    return {
        runSaveFlow: runSaveFlow
    };
});
