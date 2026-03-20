sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/UiDecisionBehaviorRuntime"
], function (UiDecisionBehaviorRuntime) {
    "use strict";

    function runOperation(sOperation, mContext) {
        return UiDecisionBehaviorRuntime.execute(sOperation, mContext || {});
    }

    return {
        confirmDeleteChecklist: function (mContext) {
            return runOperation("confirmDeleteChecklist", mContext);
        },
        guardOpenSelected: function (mContext) {
            return runOperation("guardOpenSelected", mContext);
        },
        guardCopySelection: function (mContext) {
            return runOperation("guardCopySelection", mContext);
        },
        notifySelectVisibleEmpty: function (mContext) {
            return runOperation("notifySelectVisibleEmpty", mContext);
        },
        notifyShellRefreshSuccess: function (mContext) {
            return runOperation("notifyShellRefreshSuccess", mContext);
        },
        notifyShellRefreshFailure: function (mContext) {
            return runOperation("notifyShellRefreshFailure", mContext);
        },
        notifyCorrelationCopied: function (mContext) {
            return runOperation("notifyCorrelationCopied", mContext);
        },
        registerBehaviorOverride: UiDecisionBehaviorRuntime.registerBehaviorOverride,
        unregisterBehaviorOverride: UiDecisionBehaviorRuntime.unregisterBehaviorOverride,
        clearBehaviorOverrides: UiDecisionBehaviorRuntime.clearBehaviorOverrides
    };
});
