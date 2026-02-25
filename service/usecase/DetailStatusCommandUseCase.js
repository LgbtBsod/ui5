sap.ui.define([], function () {
    "use strict";

    function runStatusChangeFlow(mArgs) {
        var sStatus = mArgs.targetStatus;
        if (!sStatus) {
            return Promise.resolve(null);
        }

        return mArgs.validateChecklist().then(function (bValid) {
            if (!bValid) {
                return null;
            }

            var oRoot = mArgs.getSelectedRoot() || {};
            if (!mArgs.shouldApplyStatusChange(oRoot.status, sStatus)) {
                return null;
            }

            if (!mArgs.requiresIntegrationConfirmation(oRoot)) {
                return mArgs.applyStatusAndSave(sStatus);
            }

            return mArgs.confirmIntegrationEdit().then(function (bConfirmed) {
                if (!bConfirmed) {
                    return null;
                }
                return mArgs.applyStatusAndSave(sStatus);
            });
        });
    }

    function handleDeleteRowResult(oResult, fnShouldSync, fnSync) {
        if (!fnShouldSync(oResult)) {
            return false;
        }
        fnSync();
        return true;
    }

    return {
        runStatusChangeFlow: runStatusChangeFlow,
        handleDeleteRowResult: handleDeleteRowResult
    };
});
