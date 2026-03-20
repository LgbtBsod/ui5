sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/UiDecisionBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (UiDecisionBehaviorHelpers, BehaviorRegistry) {
    "use strict";

    var UI_DECISION_SCOPE = "uiDecision";
    var bDefaultsRegistered = false;

    function runOptionalHandler(fnHandler) {
        if (typeof fnHandler !== "function") {
            return undefined;
        }
        return fnHandler();
    }

    function confirmDeleteChecklist(mContext) {
        if (!mContext || !mContext.armed || mContext.busy) {
            return Promise.resolve(false);
        }
        return UiDecisionBehaviorHelpers.confirmDelete(
            mContext && mContext.controller,
            String((mContext && mContext.textKey) || "deleteChecklistConfirmText")
        ).then(function (sAction) {
            if (sAction !== "Delete") {
                runOptionalHandler(mContext && mContext.onReset);
                return false;
            }
            runOptionalHandler(mContext && mContext.onReset);
            return Promise.resolve(runOptionalHandler(mContext && mContext.onConfirm)).then(function () {
                return true;
            });
        });
    }

    function guardOpenSelected(mContext) {
        var oController = mContext && mContext.controller;
        var iSelectionCount = Number(mContext && mContext.selectionCount || 0);
        var sSelectedRowId = String((mContext && mContext.selectedRowId) || "").trim();
        if (!sSelectedRowId) {
            UiDecisionBehaviorHelpers.showError(oController, "nothingToOpen");
            runOptionalHandler(mContext && mContext.onMissingSelection);
            return false;
        }
        if (iSelectionCount > 1) {
            UiDecisionBehaviorHelpers.showToast(oController, "searchOpenUsesFirstHint", [iSelectionCount]);
        }
        return true;
    }

    function guardCopySelection(mContext) {
        var oController = mContext && mContext.controller;
        var iSelectionCount = Number(mContext && mContext.selectionCount || 0);
        if (iSelectionCount > 1) {
            UiDecisionBehaviorHelpers.showError(oController, "searchCopySingleSelectionHint");
            runOptionalHandler(mContext && mContext.onBlockedSelection);
            return false;
        }
        return true;
    }

    function notifySelectVisibleEmpty(mContext) {
        UiDecisionBehaviorHelpers.showError(mContext && mContext.controller, "searchSelectVisibleEmpty");
        return false;
    }

    function notifyShellRefreshSuccess(mContext) {
        UiDecisionBehaviorHelpers.showToast(mContext && mContext.controller, "shellContextRefreshed");
        return true;
    }

    function notifyShellRefreshFailure(mContext) {
        var oError = mContext && mContext.error;
        UiDecisionBehaviorHelpers.showToast(mContext && mContext.controller, "shellUserRefreshFailed", [oError && oError.message || "Unknown error"]);
        return false;
    }

    function notifyCorrelationCopied(mContext) {
        UiDecisionBehaviorHelpers.showToast(mContext && mContext.controller, "correlationIdCopied");
        return true;
    }

    var mHandlers = {
        confirmDeleteChecklist: confirmDeleteChecklist,
        guardOpenSelected: guardOpenSelected,
        guardCopySelection: guardCopySelection,
        notifySelectVisibleEmpty: notifySelectVisibleEmpty,
        notifyShellRefreshSuccess: notifyShellRefreshSuccess,
        notifyShellRefreshFailure: notifyShellRefreshFailure,
        notifyCorrelationCopied: notifyCorrelationCopied
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(UI_DECISION_SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
