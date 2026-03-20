sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DialogOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/FeedbackBehaviorHelpers"
], function (DialogOrchestrator, FeedbackBehaviorHelpers) {
    "use strict";

    function showToast(oController, sTextKey, aArgs) {
        if (oController && typeof oController.showI18nToast === "function") {
            oController.showI18nToast(sTextKey, aArgs || []);
        }
    }

    function showError(oController, sTextKey, aArgs) {
        if (oController && typeof oController.showI18nError === "function") {
            oController.showI18nError(sTextKey, aArgs || []);
        }
    }

    function confirmDelete(oController, sTextKey) {
        return DialogOrchestrator.promptWarning(
            FeedbackBehaviorHelpers.resolveText(oController, sTextKey || "deleteChecklistConfirmText", [], sTextKey || "deleteChecklistConfirmText"),
            [DialogOrchestrator.actions.DELETE, DialogOrchestrator.actions.CANCEL],
            DialogOrchestrator.actions.CANCEL
        );
    }

    return Object.freeze({
        showToast: showToast,
        showError: showError,
        confirmDelete: confirmDelete
    });
});
