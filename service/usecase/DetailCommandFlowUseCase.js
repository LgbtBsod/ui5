sap.ui.define([], function () {
    "use strict";

    function shouldPromptBeforeClose(bIsDirty) {
        return !!bIsDirty;
    }

    function shouldProceedAfterUnsavedDecision(sDecision) {
        return sDecision === "DISCARD" || sDecision === "SAVE";
    }

    function shouldPromptBeforeDisableEdit(bEditMode, bIsDirty) {
        return !bEditMode && !!bIsDirty;
    }

    function isCancelDecision(sDecision) {
        return sDecision === "CANCEL";
    }

    return {
        shouldPromptBeforeClose: shouldPromptBeforeClose,
        shouldProceedAfterUnsavedDecision: shouldProceedAfterUnsavedDecision,
        shouldPromptBeforeDisableEdit: shouldPromptBeforeDisableEdit,
        isCancelDecision: isCancelDecision
    };
});
