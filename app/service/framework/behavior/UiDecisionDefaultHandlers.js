sap.ui.define([
    "checklist/app/service/framework/DialogOrchestrator",
    "checklist/app/service/framework/EffectTextResolver",
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (DialogOrchestrator, EffectTextResolver, BehaviorRegistry) {
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
        var oController = mContext && mContext.controller;
        var sTextKey = String((mContext && mContext.textKey) || "deleteChecklistConfirmText");
        var sText;
        if (!mContext || !mContext.armed || mContext.busy) {
            return Promise.resolve(false);
        }
        sText = EffectTextResolver.getText(oController, sTextKey, [], sTextKey);
        return DialogOrchestrator.promptWarning(
            sText,
            [DialogOrchestrator.actions.DELETE, DialogOrchestrator.actions.CANCEL],
            DialogOrchestrator.actions.CANCEL
        ).then(function (sAction) {
            if (sAction !== DialogOrchestrator.actions.DELETE) {
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
            if (oController && typeof oController.showI18nError === "function") {
                oController.showI18nError("nothingToOpen");
            }
            runOptionalHandler(mContext && mContext.onMissingSelection);
            return false;
        }
        if (iSelectionCount > 1 && oController && typeof oController.showI18nToast === "function") {
            oController.showI18nToast("searchOpenUsesFirstHint", [iSelectionCount]);
        }
        return true;
    }

    function guardCopySelection(mContext) {
        var oController = mContext && mContext.controller;
        var iSelectionCount = Number(mContext && mContext.selectionCount || 0);
        if (iSelectionCount > 1) {
            if (oController && typeof oController.showI18nError === "function") {
                oController.showI18nError("searchCopySingleSelectionHint");
            }
            runOptionalHandler(mContext && mContext.onBlockedSelection);
            return false;
        }
        return true;
    }

    function notifySelectVisibleEmpty(mContext) {
        var oController = mContext && mContext.controller;
        if (oController && typeof oController.showI18nError === "function") {
            oController.showI18nError("searchSelectVisibleEmpty");
        }
        return false;
    }

    function notifyShellRefreshSuccess(mContext) {
        var oController = mContext && mContext.controller;
        if (oController && typeof oController.showI18nToast === "function") {
            oController.showI18nToast("shellContextRefreshed");
        }
        return true;
    }

    function notifyShellRefreshFailure(mContext) {
        var oController = mContext && mContext.controller;
        var oError = mContext && mContext.error;
        if (oController && typeof oController.showI18nToast === "function") {
            oController.showI18nToast("shellUserRefreshFailed", [oError && oError.message || "Unknown error"]);
        }
        return false;
    }

    function notifyCorrelationCopied(mContext) {
        var oController = mContext && mContext.controller;
        if (oController && typeof oController.showI18nToast === "function") {
            oController.showI18nToast("correlationIdCopied");
        }
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
