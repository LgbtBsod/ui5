sap.ui.define([
    "sap_ui5/service/usecase/DetailSaveConflictFlowUseCase"
], function (DetailSaveConflictFlowUseCase) {
    "use strict";

    function createConflictAdapter(mArgs) {
        return DetailSaveConflictFlowUseCase.buildConflictHandler({
            reloadLabel: mArgs.reloadLabel,
            overwriteLabel: mArgs.overwriteLabel,
            onReload: mArgs.onReload,
            onOverwrite: mArgs.onOverwrite
        });
    }

    function createBackendErrorAdapter(mArgs) {
        return {
            onConflictChoice: createConflictAdapter(mArgs)
        };
    }

    function handleSaveError(mArgs) {
        var mAdapter = createBackendErrorAdapter(mArgs);
        return mArgs.handleBackendError(mArgs.host, mArgs.error, mAdapter);
    }

    return {
        createConflictAdapter: createConflictAdapter,
        createBackendErrorAdapter: createBackendErrorAdapter,
        handleSaveError: handleSaveError
    };
});
