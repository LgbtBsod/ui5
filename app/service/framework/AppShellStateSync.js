sap.ui.define([
    "sap_ui5/service/framework/LazyDialogRuntime"
], function (LazyDialogRuntime) {
    "use strict";

    function resolveStateModel(oController) {
        return oController.getModel("state") || (oController.getOwnerComponent && oController.getOwnerComponent() && oController.getOwnerComponent().getModel("state"));
    }

    function ensureControllerStateModel(oController, oStateModel) {
        if (oStateModel && !oController.getModel("state")) { oController.getView().setModel(oStateModel, "state"); }
    }

    function openTestUserDialog(oController) {
        if (oController._oTestUserDialog) {
            if (typeof oController._oTestUserDialog.open === "function") {
                oController._oTestUserDialog.open();
            }
            return Promise.resolve(oController._oTestUserDialog);
        }
        return LazyDialogRuntime.ensureDialog(oController, "testUser", {
            fragmentName: "sap_ui5.view.fragment.TestUserDialog",
            cacheProperty: "_mShellOverlays"
        }).then(function (oDialog) {
            oController._oTestUserDialog = oDialog;
            if (oDialog && typeof oDialog.open === "function") {
                oDialog.open();
            }
            return oDialog;
        });
    }

    function closeTestUserDialog(oController) {
        if (oController && oController._oTestUserDialog && typeof oController._oTestUserDialog.close === "function") {
            oController._oTestUserDialog.close();
            return true;
        }
        return false;
    }

    function syncTestUserDialogState(oController) {
        var oState = oController.getModel("state");
        if (!oState) { return; }
        if (oState.getProperty("/requiresUserLogin")) { openTestUserDialog(oController); return; }
        closeTestUserDialog(oController);
    }

    return {
        resolveStateModel: resolveStateModel,
        ensureControllerStateModel: ensureControllerStateModel,
        openTestUserDialog: openTestUserDialog,
        closeTestUserDialog: closeTestUserDialog,
        syncTestUserDialogState: syncTestUserDialogState
    };
});
