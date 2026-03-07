sap.ui.define([
    "sap_ui5/service/framework/ActionContract"
], function (ActionContract) {
    "use strict";

    function runPrimaryAction(oController, vActionKind, oEvent) {
        var sAction = ActionContract.normalizeShellUserAction(vActionKind);
        if (sAction === ActionContract.SHELL_USER_ACTIONS.TEST_USER) {
            return Promise.resolve(oController.onOpenTestUserDialog(oEvent));
        }
        if (sAction === ActionContract.SHELL_USER_ACTIONS.REFRESH_CONTEXT) {
            return Promise.resolve(oController._refreshShellUserContext());
        }
        return Promise.resolve();
    }

    return {
        runPrimaryAction: runPrimaryAction
    };
});
