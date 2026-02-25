sap.ui.define([], function () {
    "use strict";

    function runCloseFlow(mArgs) {
        if (!mArgs.shouldPromptBeforeClose(mArgs.isDirty)) {
            mArgs.proceed();
            return Promise.resolve(true);
        }

        return mArgs.confirmUnsaved().then(function (sDecision) {
            if (mArgs.shouldProceedAfterUnsavedDecision(sDecision)) {
                mArgs.proceed();
                return true;
            }
            return false;
        });
    }

    return {
        runCloseFlow: runCloseFlow
    };
});
