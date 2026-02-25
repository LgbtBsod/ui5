sap.ui.define([
    "sap_ui5/service/usecase/DetailDictionarySelectionUseCase"
], function (DetailDictionarySelectionUseCase) {
    "use strict";

    function shouldPromptWarning(mArgs) {
        return DetailDictionarySelectionUseCase.shouldConfirmBarrierReset({
            barrierAllowed: mArgs && mArgs.barrierAllowed,
            barriers: (mArgs && mArgs.barriers) || []
        });
    }

    function applyWarningDecision(mArgs) {
        var oSelectedModel = mArgs && mArgs.selectedModel;
        if (!oSelectedModel || typeof oSelectedModel.setProperty !== "function") {
            return false;
        }

        DetailDictionarySelectionUseCase.applyLpcDecision({
            confirmed: !!(mArgs && mArgs.confirmed),
            selectedModel: oSelectedModel
        });

        if (typeof (mArgs && mArgs.onAfterApply) === "function") {
            mArgs.onAfterApply();
        }
        return true;
    }

    function openWarningDialog(mArgs) {
        var oMessageBox = mArgs && mArgs.messageBox;
        if (!oMessageBox || typeof oMessageBox.warning !== "function") {
            return Promise.resolve(false);
        }

        var bShouldPrompt = shouldPromptWarning({
            barrierAllowed: mArgs && mArgs.barrierAllowed,
            barriers: mArgs && mArgs.barriers
        });
        if (!bShouldPrompt) {
            return Promise.resolve(false);
        }

        return new Promise(function (resolve) {
            oMessageBox.warning((mArgs && mArgs.promptText) || "", {
                actions: [oMessageBox.Action.YES, oMessageBox.Action.NO],
                emphasizedAction: oMessageBox.Action.NO,
                onClose: function (sAction) {
                    var bApplied = applyWarningDecision({
                        confirmed: sAction === oMessageBox.Action.YES,
                        selectedModel: mArgs && mArgs.selectedModel,
                        onAfterApply: mArgs && mArgs.onAfterApply
                    });
                    resolve(bApplied);
                }
            });
        });
    }

    return {
        shouldPromptWarning: shouldPromptWarning,
        applyWarningDecision: applyWarningDecision,
        openWarningDialog: openWarningDialog
    };
});
