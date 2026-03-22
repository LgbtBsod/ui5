sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract"
], function (ActionContract) {
    "use strict";

    function retryScope() {
        return sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes").retry;
    }

    function runOperation(sOperation, mContext) {
        return retryScope().execute(sOperation, mContext || {});
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
        registerBehaviorOverride: function () {
            return retryScope().registerBehaviorOverride.apply(null, arguments);
        },
        unregisterBehaviorOverride: function () {
            return retryScope().unregisterBehaviorOverride.apply(null, arguments);
        },
        clearBehaviorOverrides: function () {
            return retryScope().clearBehaviorOverrides.apply(null, arguments);
        }
    };
});
