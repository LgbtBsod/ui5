sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerCommandContextRuntime"
], function (ControllerCommandRuntime, RuntimePayloadNormalizer, ControllerCommandContextRuntime) {
    "use strict";

    function normalizePayload(mInput) {
        return RuntimePayloadNormalizer.normalize(mInput);
    }

    function execute(oController, sMethod, mInput) {
        return ControllerCommandRuntime.executeUseCaseCommand(
            oController,
            oController && oController._detailService,
            sMethod,
            normalizePayload(mInput || {}),
            ControllerCommandContextRuntime.buildDefaultCtx(oController)
        );
    }

    return Object.freeze({
        enterEdit: function (oController, mInput) {
            return execute(oController, "enterEdit", mInput);
        },
        open: function (oController, mInput) {
            return execute(oController, "open", mInput);
        },
        save: function (oController, mInput) {
            return execute(oController, "save", mInput);
        },
        close: function (oController, mInput) {
            return execute(oController, "close", mInput);
        },
        deleteChecklist: function (oController, mInput) {
            return execute(oController, "deleteChecklist", mInput);
        },
        discardChanges: function (oController, mInput) {
            return execute(oController, "discardChanges", mInput);
        },
        validate: function (oController, mInput) {
            return execute(oController, "validate", mInput);
        },
        changeStatus: function (oController, mInput) {
            return execute(oController, "changeStatus", mInput);
        },
        rowOps: function (oController, mInput) {
            return execute(oController, "rowOps", mInput);
        },
        resolveConflict: function (oController, mInput) {
            return execute(oController, "resolveConflict", mInput);
        },
        attachmentLoad: function (oController, mInput) {
            return execute(oController, "attachmentLoad", mInput);
        },
        attachmentDelete: function (oController, mInput) {
            return execute(oController, "attachmentDelete", mInput);
        },
        attachmentUpload: function (oController, mInput) {
            return execute(oController, "attachmentUpload", mInput);
        },
        valueHelpLocation: function (oController, mInput) {
            return execute(oController, "valueHelpLocation", mInput);
        },
        autosave: function (oController, mInput) {
            return execute(oController, "autosave", mInput);
        },
        personSuggest: function (oController, mInput) {
            return execute(oController, "personSuggest", mInput);
        }
    });
});
