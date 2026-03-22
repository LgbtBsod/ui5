sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes"
], function (ActionContract, BehaviorScopes) {
    "use strict";

    function runOperation(sOperation, mContext) {
        return BehaviorScopes.retry.execute(sOperation, mContext || {});
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
        registerBehaviorOverride: BehaviorScopes.retry.registerBehaviorOverride,
        unregisterBehaviorOverride: BehaviorScopes.retry.unregisterBehaviorOverride,
        clearBehaviorOverrides: BehaviorScopes.retry.clearBehaviorOverrides
    };
});
