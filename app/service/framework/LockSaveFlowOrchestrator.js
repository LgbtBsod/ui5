sap.ui.define([
    "checklist/app/service/framework/DialogOrchestrator",
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/RootIdRuntime"
], function (DialogOrchestrator, DetailCommandPolicy, ModelStateRuntime, RootIdRuntime) {
    "use strict";

    function resetDeleteChecklistConfirmArmed(oController) {
        ModelStateRuntime.write(oController, "view", "/deleteChecklistConfirmArmed", false);
    }

    function toggleEdit(oController, oEvent) {
        return Promise.resolve(DetailCommandPolicy.enterEdit(oController, RootIdRuntime.withCurrentRootId(oController, {
            state: !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"))
        }))).finally(function () {
            if (oController && typeof oController._scheduleAttachmentDropZoneBind === "function") {
                oController._scheduleAttachmentDropZoneBind();
            }
        });
    }

    function save(oController, mOptions) {
        var sSaveInFlightPath = (mOptions && mOptions.saveInFlightPath) || "/saveInFlight";
        if (ModelStateRuntime.any(oController, "state", ["/isBusy", sSaveInFlightPath])) {
            return Promise.resolve(false);
        }
        return ModelStateRuntime.withFlags(oController, "state", [sSaveInFlightPath, "/isBusy"], function () {
            return DetailCommandPolicy.save(oController, RootIdRuntime.withCurrentRootId(oController));
        });
    }

    function close(oController) {
        resetDeleteChecklistConfirmArmed(oController);
        return DetailCommandPolicy.close(oController, RootIdRuntime.withCurrentRootId(oController));
    }

    function armDelete(oController) {
        var bCurrent;
        if (ModelStateRuntime.any(oController, "state", ["/isBusy", "/lockOperationPending"])) {
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
        if (!bArmed || ModelStateRuntime.any(oController, "state", ["/isBusy", "/lockOperationPending"])) {
            return Promise.resolve(false);
        }
        return DialogOrchestrator.promptWarning(
            sText,
            [DialogOrchestrator.actions.DELETE, DialogOrchestrator.actions.CANCEL],
            DialogOrchestrator.actions.CANCEL
        ).then(function (sAction) {
            if (sAction !== DialogOrchestrator.actions.DELETE) {
                resetDeleteChecklistConfirmArmed(oController);
                return false;
            }
            resetDeleteChecklistConfirmArmed(oController);
            return ModelStateRuntime.withFlag(oController, "state", "/isBusy", function () {
                return DetailCommandPolicy.deleteChecklist(oController, RootIdRuntime.withCurrentRootId(oController));
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
