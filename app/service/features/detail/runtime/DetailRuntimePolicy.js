sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRuntimeConstants"
], function (DetailRuntimeConstants) {
    "use strict";

    function detailRuntimeScope() {
        return sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes").detailRuntime;
    }

    function runSyncOperation(sOperation, mContext) {
        return detailRuntimeScope().executeSync(sOperation, mContext || {});
    }

    function analyticsEditRestorePlan(mContext) {
        var oPlan = runSyncOperation(DetailRuntimeConstants.OP_ANALYTICS_EDIT_RESTORE, mContext);
        var iMaxAttempts = Number(oPlan && oPlan.maxAttempts);
        var iRetryDelay = Number(oPlan && oPlan.retryDelayMs);

        return {
            maxAttempts: Number.isFinite(iMaxAttempts) && iMaxAttempts > 0 ? iMaxAttempts : 3,
            retryDelayMs: Number.isFinite(iRetryDelay) && iRetryDelay >= 0 ? iRetryDelay : 220
        };
    }

    return {
        analyticsEditRestorePlan: analyticsEditRestorePlan,
        registerBehaviorOverride: function () {
            return detailRuntimeScope().registerBehaviorOverride.apply(null, arguments);
        },
        unregisterBehaviorOverride: function () {
            return detailRuntimeScope().unregisterBehaviorOverride.apply(null, arguments);
        },
        clearBehaviorOverrides: function () {
            return detailRuntimeScope().clearBehaviorOverrides.apply(null, arguments);
        }
    };
});
