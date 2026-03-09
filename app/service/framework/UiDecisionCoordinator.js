sap.ui.define([
    "checklist/app/service/framework/behavior/BehaviorResolver",
    "checklist/app/service/framework/behavior/UiDecisionDefaultHandlers",
    "checklist/app/service/framework/behavior/UiDecisionOverrideHandlers"
], function (BehaviorResolver, UiDecisionDefaultHandlers, UiDecisionOverrideHandlers) {
    "use strict";

    function runOperation(sOperation, mContext) {
        UiDecisionDefaultHandlers.ensureRegistered();
        UiDecisionOverrideHandlers.ensureRegistered();
        return BehaviorResolver.execute("uiDecision", sOperation, mContext || {}, UiDecisionDefaultHandlers.handlers);
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
        registerBehaviorOverride: UiDecisionOverrideHandlers.register,
        unregisterBehaviorOverride: UiDecisionOverrideHandlers.unregister,
        clearBehaviorOverrides: UiDecisionOverrideHandlers.clear
    };
});
