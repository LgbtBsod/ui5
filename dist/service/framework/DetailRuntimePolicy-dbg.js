sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DetailRuntimeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DetailRuntimeDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/DetailRuntimeOverrideHandlers"
], function (DetailRuntimeConstants, BehaviorResolver, DetailRuntimeDefaultHandlers, DetailRuntimeOverrideHandlers) {
    "use strict";

    function runSyncOperation(sOperation, mContext) {
        DetailRuntimeDefaultHandlers.ensureRegistered();
        DetailRuntimeOverrideHandlers.ensureRegistered();
        return BehaviorResolver.executeSync(
            DetailRuntimeConstants.SCOPE,
            sOperation,
            mContext || {},
            DetailRuntimeDefaultHandlers.handlers
        );
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
        registerBehaviorOverride: DetailRuntimeOverrideHandlers.register,
        unregisterBehaviorOverride: DetailRuntimeOverrideHandlers.unregister,
        clearBehaviorOverrides: DetailRuntimeOverrideHandlers.clear
    };
});
