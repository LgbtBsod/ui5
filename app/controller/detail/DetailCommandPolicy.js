sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FacadeCommandConstants"
], function (ControllerCommandRuntime, RuntimePayloadNormalizer, ControllerCommandContextRuntime, FacadeCommandConstants) {
    "use strict";

    var COMMAND = FacadeCommandConstants.DETAIL;

    function normalizePayload(mInput) {
        return RuntimePayloadNormalizer.normalize(mInput);
    }

    function execute(oController, sMethod, mInput) {
        return ControllerCommandRuntime.executeCommand(
            oController,
            oController && oController._detailService,
            sMethod,
            normalizePayload(mInput || {}),
            ControllerCommandContextRuntime.buildCtx(oController)
        );
    }

    return Object.freeze({
        enterEdit: function (oController, mInput) {
            return execute(oController, COMMAND.ENTER_EDIT, mInput);
        },
        open: function (oController, mInput) {
            return execute(oController, COMMAND.OPEN, mInput);
        },
        save: function (oController, mInput) {
            return execute(oController, COMMAND.SAVE, mInput);
        },
        close: function (oController, mInput) {
            return execute(oController, COMMAND.CLOSE, mInput);
        },
        deleteChecklist: function (oController, mInput) {
            return execute(oController, COMMAND.DELETE_CHECKLIST, mInput);
        },
        discardChanges: function (oController, mInput) {
            return execute(oController, COMMAND.DISCARD_CHANGES, mInput);
        },
        validate: function (oController, mInput) {
            return execute(oController, COMMAND.VALIDATE, mInput);
        },
        rowOps: function (oController, mInput) {
            return execute(oController, COMMAND.ROW_OPS, mInput);
        },
        resolveConflict: function (oController, mInput) {
            return execute(oController, COMMAND.RESOLVE_CONFLICT, mInput);
        },
        attachmentLoad: function (oController, mInput) {
            return execute(oController, COMMAND.ATTACHMENT_LOAD, mInput);
        },
        attachmentDelete: function (oController, mInput) {
            return execute(oController, COMMAND.ATTACHMENT_DELETE, mInput);
        },
        attachmentUpload: function (oController, mInput) {
            return execute(oController, COMMAND.ATTACHMENT_UPLOAD, mInput);
        },
        valueHelpLocation: function (oController, mInput) {
            return execute(oController, COMMAND.VALUE_HELP_LOCATION, mInput);
        },
        autosave: function (oController, mInput) {
            return execute(oController, COMMAND.AUTOSAVE, mInput);
        },
        personSuggest: function (oController, mInput) {
            return execute(oController, COMMAND.PERSON_SUGGEST, mInput);
        }
    });
});
