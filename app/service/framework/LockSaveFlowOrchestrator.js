sap.ui.define([
    "checklist/app/service/framework/DialogOrchestrator",
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/RootIdRuntime"
], function (DialogOrchestrator, DetailCommandPolicy, ModelStateRuntime, RootIdRuntime) {
    "use strict";

    function toggleEdit(oController, oEvent) {
        return Promise.resolve(DetailCommandPolicy.enterEdit(oController, {
            state: !!(oEvent && oEvent.getParameter && oEvent.getParameter("state")),
            rootId: RootIdRuntime.resolveCurrentRootId(oController)
        })).finally(function () {
            if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
                oController._scheduleAttachmentDropZoneBind();
            }
        });
    }

    function save(oController, mOptions) {
        var sSaveInFlightPath = (mOptions && mOptions.saveInFlightPath) || "/saveInFlight";
        if (ModelStateRuntime.read(oController, "state", "/isBusy") || ModelStateRuntime.read(oController, "state", sSaveInFlightPath)) {
            return Promise.resolve(false);
        }
        ModelStateRuntime.write(oController, "state", sSaveInFlightPath, true);
        ModelStateRuntime.write(oController, "state", "/isBusy", true);
        return Promise.resolve(DetailCommandPolicy.save(oController, { rootId: RootIdRuntime.resolveCurrentRootId(oController) })).finally(function () {
            ModelStateRuntime.write(oController, "state", sSaveInFlightPath, false);
            if (ModelStateRuntime.read(oController, "state", "/isBusy")) {
                ModelStateRuntime.write(oController, "state", "/isBusy", false);
            }
        });
    }

    function close(oController) {
        if (oController && typeof oController._setDeleteChecklistConfirmArmed === "function") {
            oController._setDeleteChecklistConfirmArmed(false);
        }
        return DetailCommandPolicy.close(oController, { rootId: RootIdRuntime.resolveCurrentRootId(oController) });
    }

    function armDelete(oController) {
        var bCurrent;
        if (ModelStateRuntime.read(oController, "state", "/isBusy") || ModelStateRuntime.read(oController, "state", "/lockOperationPending")) {
            return Promise.resolve(false);
        }
        bCurrent = !!ModelStateRuntime.read(oController, "view", "/deleteChecklistConfirmArmed", false);
        ModelStateRuntime.write(oController, "view", "/deleteChecklistConfirmArmed", !bCurrent);
        return Promise.resolve(true);
    }

    function confirmDelete(oController) {
        var bArmed = !!ModelStateRuntime.read(oController, "view", "/deleteChecklistConfirmArmed", false);
        var oBundle = oController && oController.getResourceBundle && oController.getResourceBundle();
        var sText = oBundle && oBundle.getText ? oBundle.getText("deleteChecklistConfirmText") : "deleteChecklistConfirmText";
        if (!bArmed || ModelStateRuntime.read(oController, "state", "/isBusy") || ModelStateRuntime.read(oController, "state", "/lockOperationPending")) {
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
                    ModelStateRuntime.write(oController, "view", "/deleteChecklistConfirmArmed", false);
                }
                return false;
            }
            if (oController && typeof oController._setDeleteChecklistConfirmArmed === "function") {
                oController._setDeleteChecklistConfirmArmed(false);
            } else {
                ModelStateRuntime.write(oController, "view", "/deleteChecklistConfirmArmed", false);
            }
            return ModelStateRuntime.withFlag(oController, "state", "/isBusy", function () {
                return DetailCommandPolicy.deleteChecklist(oController, { rootId: RootIdRuntime.resolveCurrentRootId(oController) });
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
