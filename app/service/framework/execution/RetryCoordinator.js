sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/RetryBehaviorRuntime"
], function (ActionContract, RetryBehaviorRuntime) {
    "use strict";

    function runOperation(sOperation, mContext) {
        return RetryBehaviorRuntime.execute(sOperation, mContext || {});
    }

    function runRetry(oController, vRetryAction) {
        var sAction = ActionContract.normalizeRetryAction(vRetryAction);
        if (!sAction) {
            return Promise.resolve();
        }
        return runOperation(sAction, {
            controller: oController,
            action: sAction
        });
    }

    return {
        runRetry: runRetry,
        registerBehaviorOverride: RetryBehaviorRuntime.registerBehaviorOverride,
        unregisterBehaviorOverride: RetryBehaviorRuntime.unregisterBehaviorOverride,
        clearBehaviorOverrides: RetryBehaviorRuntime.clearBehaviorOverrides
    };
});
