sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandRuntime"
], function (FacadeCommandRuntime) {
    "use strict";

    function execute(oController, sMethod, mInput) {
        return FacadeCommandRuntime.executeDetail(
            oController,
            oController && oController._facade,
            sMethod,
            mInput || {}
        );
    }

    function enterEdit(oController, mInput) {
        return execute(oController, "enterEdit", mInput);
    }

    function open(oController, mInput) {
        return execute(oController, "open", mInput);
    }

    function save(oController, mInput) {
        return execute(oController, "save", mInput);
    }

    function close(oController, mInput) {
        return execute(oController, "close", mInput);
    }

    function deleteChecklist(oController, mInput) {
        return execute(oController, "deleteChecklist", mInput);
    }

    function discardChanges(oController, mInput) {
        return execute(oController, "discardChanges", mInput);
    }

    function validate(oController, mInput) {
        return execute(oController, "validate", mInput);
    }

    function changeStatus(oController, mInput) {
        return execute(oController, "changeStatus", mInput);
    }

    function rowOps(oController, mInput) {
        return execute(oController, "rowOps", mInput);
    }

    function resolveConflict(oController, mInput) {
        return execute(oController, "resolveConflict", mInput);
    }

    function attachmentLoad(oController, mInput) {
        return execute(oController, "attachmentLoad", mInput);
    }

    function attachmentDelete(oController, mInput) {
        return execute(oController, "attachmentDelete", mInput);
    }

    function attachmentUpload(oController, mInput) {
        return execute(oController, "attachmentUpload", mInput);
    }

    function valueHelpLocation(oController, mInput) {
        return execute(oController, "valueHelpLocation", mInput);
    }

    function autosave(oController, mInput) {
        return execute(oController, "autosave", mInput);
    }

    function personSuggest(oController, mInput) {
        return execute(oController, "personSuggest", mInput);
    }

    return {
        enterEdit: enterEdit,
        open: open,
        save: save,
        close: close,
        deleteChecklist: deleteChecklist,
        discardChanges: discardChanges,
        validate: validate,
        changeStatus: changeStatus,
        rowOps: rowOps,
        resolveConflict: resolveConflict,
        attachmentLoad: attachmentLoad,
        attachmentDelete: attachmentDelete,
        attachmentUpload: attachmentUpload,
        valueHelpLocation: valueHelpLocation,
        autosave: autosave,
        personSuggest: personSuggest
    };
});
