sap.ui.define([], function () {
    "use strict";

    function runRetry(mArgs) {
        return mArgs.runRetryLifecycle({
            beginLatency: mArgs.beginLatency,
            runRetryFlow: function () {
                return mArgs.runRetryFlow({
                    stateModel: mArgs.stateModel,
                    dataModel: mArgs.dataModel,
                    runWithLoading: mArgs.runWithLoading,
                    getCheckLists: mArgs.getCheckLists,
                    onAfterApply: mArgs.onAfterApply,
                    treatEmptyAsError: false,
                    maxAttempts: 2
                });
            },
            presentRetryOutcome: mArgs.presentRetryOutcome,
            markRetryFailure: mArgs.markRetryFailure,
            finishLatency: mArgs.finishLatency,
            afterRetryApplied: mArgs.afterRetryApplied
        }).then(function (oLifecycleResult) {
            return (oLifecycleResult && oLifecycleResult.result) || oLifecycleResult;
        });
    }

    return {
        runRetry: runRetry
    };
});
