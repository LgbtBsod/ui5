sap.ui.define([
    "checklist/app/service/framework/ActionContract",
    "checklist/app/service/framework/behavior/BehaviorResolver",
    "checklist/app/service/framework/behavior/RetryDefaultHandlers",
    "checklist/app/service/framework/behavior/RetryOverrideHandlers"
], function (ActionContract, BehaviorResolver, RetryDefaultHandlers, RetryOverrideHandlers) {
    "use strict";

    function runOperation(sOperation, mContext) {
        RetryDefaultHandlers.ensureRegistered();
        RetryOverrideHandlers.ensureRegistered();
        return BehaviorResolver.execute("retry", sOperation, mContext || {}, RetryDefaultHandlers.handlers);
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
        registerBehaviorOverride: RetryOverrideHandlers.register,
        unregisterBehaviorOverride: RetryOverrideHandlers.unregister,
        clearBehaviorOverrides: RetryOverrideHandlers.clear
    };
});
