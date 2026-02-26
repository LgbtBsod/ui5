sap.ui.define([], function () {
    "use strict";

    function resolveSelectedKey(oEvent) {
        var oSelectedItem = oEvent && oEvent.getParameter ? oEvent.getParameter("selectedItem") : null;
        if (oSelectedItem && typeof oSelectedItem.getKey === "function") {
            return oSelectedItem.getKey();
        }
        var oSource = oEvent && oEvent.getSource ? oEvent.getSource() : null;
        return oSource && typeof oSource.getSelectedKey === "function" ? oSource.getSelectedKey() : "";
    }

    function resolveDictionaryMatch(mArgs) {
        var aDictionary = (mArgs && mArgs.dictionary) || [];
        var sKey = String((mArgs && mArgs.key) || "");
        return aDictionary.find(function (oItem) {
            return oItem && oItem.key === sKey;
        }) || null;
    }

    function applyDictionarySelection(mArgs) {
        var oSelectedModel = mArgs && mArgs.selectedModel;
        if (!oSelectedModel || typeof oSelectedModel.setProperty !== "function") {
            return false;
        }
        var sKey = String((mArgs && mArgs.key) || "");
        var oMatch = resolveDictionaryMatch({
            dictionary: mArgs && mArgs.dictionary,
            key: sKey
        });
        oSelectedModel.setProperty((mArgs && mArgs.keyPath) || "", sKey);
        oSelectedModel.setProperty((mArgs && mArgs.textPath) || "", (oMatch && oMatch.text) || "");
        return true;
    }


    function runDictionarySelectionLifecycle(mArgs) {
        var sKey = resolveSelectedKey(mArgs && mArgs.event);
        var bApplied = applyDictionarySelection({
            key: sKey,
            dictionary: mArgs && mArgs.dictionary,
            keyPath: mArgs && mArgs.keyPath,
            textPath: mArgs && mArgs.textPath,
            selectedModel: mArgs && mArgs.selectedModel
        });
        if (bApplied && typeof (mArgs && mArgs.onAfterApply) === "function") {
            mArgs.onAfterApply();
        }
        return { ok: bApplied, reason: bApplied ? "selection_applied" : "missing_selected_model", key: sKey };
    }


    function runLpcSelectionLifecycle(mArgs) {
        runDictionarySelectionLifecycle({
            event: mArgs && mArgs.event,
            dictionary: mArgs && mArgs.dictionary,
            keyPath: mArgs && mArgs.keyPath,
            textPath: mArgs && mArgs.textPath,
            selectedModel: mArgs && mArgs.selectedModel,
            onAfterApply: mArgs && mArgs.onAfterApply
        });

        return mArgs.openWarningDialog({
            messageBox: mArgs.messageBox,
            promptText: mArgs.promptText,
            barrierAllowed: mArgs.barrierAllowed,
            barriers: mArgs.barriers,
            selectedModel: mArgs.selectedModel,
            onAfterApply: mArgs.onAfterApply
        });
    }



    function runProfessionSelectionLifecycle(mArgs) {
        return runDictionarySelectionLifecycle({
            event: mArgs && mArgs.event,
            dictionary: mArgs && mArgs.dictionary,
            keyPath: mArgs && mArgs.keyPath,
            textPath: mArgs && mArgs.textPath,
            selectedModel: mArgs && mArgs.selectedModel,
            onAfterApply: mArgs && mArgs.onAfterApply
        });
    }

    function shouldConfirmBarrierReset(mArgs) {
        var bAllowed = !!(mArgs && mArgs.barrierAllowed);
        var aBarriers = (mArgs && mArgs.barriers) || [];
        return !bAllowed && aBarriers.length > 0;
    }

    function applyLpcDecision(mArgs) {
        var oSelectedModel = mArgs && mArgs.selectedModel;
        if (!oSelectedModel || typeof oSelectedModel.setProperty !== "function") {
            return false;
        }

        if (mArgs && mArgs.confirmed) {
            oSelectedModel.setProperty("/barriers", []);
            return true;
        }

        oSelectedModel.setProperty("/basic/LPC_KEY", "");
        oSelectedModel.setProperty("/basic/LPC_TEXT", "");
        return true;
    }

    return {
        resolveSelectedKey: resolveSelectedKey,
        resolveDictionaryMatch: resolveDictionaryMatch,
        applyDictionarySelection: applyDictionarySelection,
        runDictionarySelectionLifecycle: runDictionarySelectionLifecycle,
        runLpcSelectionLifecycle: runLpcSelectionLifecycle,
        runProfessionSelectionLifecycle: runProfessionSelectionLifecycle,
        shouldConfirmBarrierReset: shouldConfirmBarrierReset,
        applyLpcDecision: applyLpcDecision
    };
});
