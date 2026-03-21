sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/UiDecisionBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailMessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchMessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ShellMessageKeyConstants"
], function (UiDecisionBehaviorHelpers, BehaviorRegistry, DetailMessageKeyConstants, SearchMessageKeyConstants, ShellMessageKeyConstants) {
    "use strict";

    var UI_DECISION_SCOPE = "uiDecision";
    var ACTION_DELETE = "Delete";
    var TEXT_DELETE_CHECKLIST_CONFIRM = DetailMessageKeyConstants.DELETE_CHECKLIST_CONFIRM;
    var TEXT_NOTHING_TO_OPEN = SearchMessageKeyConstants.NOTHING_TO_OPEN;
    var TEXT_OPEN_USES_FIRST = SearchMessageKeyConstants.OPEN_USES_FIRST_HINT;
    var TEXT_COPY_SINGLE_SELECTION = SearchMessageKeyConstants.COPY_SINGLE_SELECTION_HINT;
    var TEXT_SELECT_VISIBLE_EMPTY = SearchMessageKeyConstants.SELECT_VISIBLE_EMPTY;
    var TEXT_SHELL_REFRESH_SUCCESS = ShellMessageKeyConstants.CONTEXT_REFRESHED;
    var TEXT_SHELL_REFRESH_FAILURE = ShellMessageKeyConstants.USER_REFRESH_FAILED;
    var TEXT_CORRELATION_COPIED = ShellMessageKeyConstants.CORRELATION_COPIED;
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
            String((mContext && mContext.textKey) || TEXT_DELETE_CHECKLIST_CONFIRM)
        ).then(function (sAction) {
            if (sAction !== ACTION_DELETE) {
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
            UiDecisionBehaviorHelpers.showError(oController, TEXT_NOTHING_TO_OPEN);
            runOptionalHandler(mContext && mContext.onMissingSelection);
            return false;
        }
        if (iSelectionCount > 1) {
            UiDecisionBehaviorHelpers.showToast(oController, TEXT_OPEN_USES_FIRST, [iSelectionCount]);
        }
        return true;
    }

    function guardCopySelection(mContext) {
        var oController = mContext && mContext.controller;
        var iSelectionCount = Number(mContext && mContext.selectionCount || 0);
        if (iSelectionCount > 1) {
            UiDecisionBehaviorHelpers.showError(oController, TEXT_COPY_SINGLE_SELECTION);
            runOptionalHandler(mContext && mContext.onBlockedSelection);
            return false;
        }
        return true;
    }

    function notifySelectVisibleEmpty(mContext) {
        UiDecisionBehaviorHelpers.showError(mContext && mContext.controller, TEXT_SELECT_VISIBLE_EMPTY);
        return false;
    }

    function notifyShellRefreshSuccess(mContext) {
        UiDecisionBehaviorHelpers.showToast(mContext && mContext.controller, TEXT_SHELL_REFRESH_SUCCESS);
        return true;
    }

    function notifyShellRefreshFailure(mContext) {
        var oError = mContext && mContext.error;
        UiDecisionBehaviorHelpers.showToast(mContext && mContext.controller, TEXT_SHELL_REFRESH_FAILURE, [String((oError && oError.message) || "")]);
        return false;
    }

    function notifyCorrelationCopied(mContext) {
        UiDecisionBehaviorHelpers.showToast(mContext && mContext.controller, TEXT_CORRELATION_COPIED);
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
