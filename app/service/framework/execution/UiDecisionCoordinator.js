sap.ui.define([], function () {
    "use strict";

    function uiDecisionScope() {
        return sap.ui.requireSync("PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes").uiDecision;
    }

    function runOperation(sOperation, mContext) {
        return uiDecisionScope().execute(sOperation, mContext || {});
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
        registerBehaviorOverride: function () {
            return uiDecisionScope().registerBehaviorOverride.apply(null, arguments);
        },
        unregisterBehaviorOverride: function () {
            return uiDecisionScope().unregisterBehaviorOverride.apply(null, arguments);
        },
        clearBehaviorOverrides: function () {
            return uiDecisionScope().clearBehaviorOverrides.apply(null, arguments);
        }
    };
});
