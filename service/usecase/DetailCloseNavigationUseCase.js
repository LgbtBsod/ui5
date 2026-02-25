sap.ui.define([], function () {
    "use strict";

    function buildCloseProceedAction(mArgs) {
        return function () {
            var sObjectId = mArgs.stateModel.getProperty("/activeObjectId");
            var sSessionId = mArgs.stateModel.getProperty("/sessionId");

            mArgs.releaseLock(sObjectId, sSessionId);
            mArgs.prepareCloseNavigation(mArgs.stateModel);
            mArgs.navigateToSearch();
        };
    }

    return {
        buildCloseProceedAction: buildCloseProceedAction
    };
});
