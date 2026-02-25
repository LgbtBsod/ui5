sap.ui.define([
    "sap_ui5/service/usecase/DetailLifecycleUseCase"
], function (DetailLifecycleUseCase) {
    "use strict";

    function runReleaseFlow(mArgs) {
        DetailLifecycleUseCase.setReadUnlocked(mArgs.stateModel);
        return mArgs.releaseLock().finally(function () {
            mArgs.setLockUiIdle();
        });
    }

    return {
        runReleaseFlow: runReleaseFlow
    };
});
