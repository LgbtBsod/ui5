sap.ui.define([], function () {
    "use strict";

    function enterEdit(oController, mInput) {
        return oController._run("enterEdit", mInput || {});
    }

    function open(oController, mInput) {
        return oController._run("open", mInput || {});
    }

    function save(oController, mInput) {
        return oController._run("save", mInput || {});
    }

    function close(oController, mInput) {
        return oController._run("close", mInput || {});
    }

    function deleteChecklist(oController, mInput) {
        return oController._run("deleteChecklist", mInput || {});
    }

    function discardChanges(oController, mInput) {
        return oController._run("discardChanges", mInput || {});
    }

    function validate(oController, mInput) {
        return oController._run("validate", mInput || {});
    }

    function changeStatus(oController, mInput) {
        return oController._run("changeStatus", mInput || {});
    }

    function rowOps(oController, mInput) {
        return oController._run("rowOps", mInput || {});
    }

    function resolveConflict(oController, mInput) {
        return oController._run("resolveConflict", mInput || {});
    }

    function attachmentDelete(oController, mInput) {
        return oController._run("attachmentDelete", mInput || {});
    }

    function attachmentUpload(oController, mInput) {
        return oController._run("attachmentUpload", mInput || {});
    }

    function valueHelpLocation(oController, mInput) {
        return oController._run("valueHelpLocation", mInput || {});
    }

    function autosave(oController, mInput) {
        return oController._run("autosave", mInput || {});
    }

    function personSuggest(oController, mInput) {
        return oController._run("personSuggest", mInput || {});
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
        attachmentDelete: attachmentDelete,
        attachmentUpload: attachmentUpload,
        valueHelpLocation: valueHelpLocation,
        autosave: autosave,
        personSuggest: personSuggest
    };
});
