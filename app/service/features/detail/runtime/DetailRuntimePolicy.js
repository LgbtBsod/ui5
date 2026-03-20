sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/DetailRuntimeBehaviorRuntime"
], function (DetailRuntimeConstants, DetailRuntimeBehaviorRuntime) {
    "use strict";

    function runSyncOperation(sOperation, mContext) {
        return DetailRuntimeBehaviorRuntime.executeSync(sOperation, mContext || {});
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
        registerBehaviorOverride: DetailRuntimeBehaviorRuntime.registerBehaviorOverride,
        unregisterBehaviorOverride: DetailRuntimeBehaviorRuntime.unregisterBehaviorOverride,
        clearBehaviorOverrides: DetailRuntimeBehaviorRuntime.clearBehaviorOverrides
    };
});
