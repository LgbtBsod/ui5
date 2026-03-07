sap.ui.define([
    "checklist/app/service/framework/DialogOrchestrator"
], function (DialogOrchestrator) {
    "use strict";

    function read(oController, sModelName, sPath, vFallback) {
        var oModel = oController && oController.getModel && oController.getModel(sModelName);
        if (!oModel || typeof oModel.getProperty !== "function") {
            return vFallback;
        }
        return oModel.getProperty(sPath);
    }

    function write(oController, sModelName, sPath, vValue) {
        var oModel = oController && oController.getModel && oController.getModel(sModelName);
        if (!oModel || typeof oModel.setProperty !== "function") {
            return false;
        }
        oModel.setProperty(sPath, vValue);
        return true;
    }

    function withFlag(oController, sModelName, sPath, fnWork, vStart, vEnd) {
        write(oController, sModelName, sPath, typeof vStart === "undefined" ? true : vStart);
        return Promise.resolve().then(fnWork).finally(function () {
            write(oController, sModelName, sPath, typeof vEnd === "undefined" ? false : vEnd);
        });
    }

    function resolveRootId(oController) {
        return String((oController && oController._currentRootId && oController._currentRootId()) || "").trim();
    }

    function toggleEdit(oController, oEvent) {
        return Promise.resolve(oController._run("enterEdit", {
            state: !!(oEvent && oEvent.getParameter && oEvent.getParameter("state")),
            rootId: resolveRootId(oController)
        })).finally(function () {
            if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
                oController._scheduleAttachmentDropZoneBind();
            }
        });
    }

    function save(oController, mOptions) {
        var sSaveInFlightPath = (mOptions && mOptions.saveInFlightPath) || "/saveInFlight";
        if (read(oController, "state", "/isBusy") || read(oController, "state", sSaveInFlightPath)) {
            return Promise.resolve(false);
        }
        write(oController, "state", sSaveInFlightPath, true);
        write(oController, "state", "/isBusy", true);
        return Promise.resolve(oController._run("save", { rootId: resolveRootId(oController) })).finally(function () {
            write(oController, "state", sSaveInFlightPath, false);
            if (read(oController, "state", "/isBusy")) {
                write(oController, "state", "/isBusy", false);
            }
        });
    }

    function close(oController) {
        if (oController && typeof oController._setDeleteChecklistConfirmArmed === "function") {
            oController._setDeleteChecklistConfirmArmed(false);
        }
        return oController._run("close", { rootId: resolveRootId(oController) });
    }

    function armDelete(oController) {
        var bCurrent;
        if (read(oController, "state", "/isBusy") || read(oController, "state", "/lockOperationPending")) {
            return Promise.resolve(false);
        }
        bCurrent = !!read(oController, "view", "/deleteChecklistConfirmArmed", false);
        write(oController, "view", "/deleteChecklistConfirmArmed", !bCurrent);
        return Promise.resolve(true);
    }

    function confirmDelete(oController) {
        var bArmed = !!read(oController, "view", "/deleteChecklistConfirmArmed", false);
        var oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        var sText = oBundle && oBundle.getText ? oBundle.getText("deleteChecklistConfirmText") : "deleteChecklistConfirmText";
        if (!bArmed || read(oController, "state", "/isBusy") || read(oController, "state", "/lockOperationPending")) {
            return Promise.resolve(false);
        }
        return DialogOrchestrator.promptWarning(
            sText,
            [DialogOrchestrator.actions.DELETE, DialogOrchestrator.actions.CANCEL],
            DialogOrchestrator.actions.CANCEL
        ).then(function (sAction) {
            if (sAction !== DialogOrchestrator.actions.DELETE) {
                if (oController && typeof oController._setDeleteChecklistConfirmArmed === "function") {
                    oController._setDeleteChecklistConfirmArmed(false);
                } else {
                    write(oController, "view", "/deleteChecklistConfirmArmed", false);
                }
                return false;
            }
            if (oController && typeof oController._setDeleteChecklistConfirmArmed === "function") {
                oController._setDeleteChecklistConfirmArmed(false);
            } else {
                write(oController, "view", "/deleteChecklistConfirmArmed", false);
            }
            return withFlag(oController, "state", "/isBusy", function () {
                return oController._run("deleteChecklist", { rootId: resolveRootId(oController) });
            }, true, false);
        });
    }

    return {
        toggleEdit: toggleEdit,
        save: save,
        close: close,
        armDelete: armDelete,
        confirmDelete: confirmDelete
    };
});
