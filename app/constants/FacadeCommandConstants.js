sap.ui.define([], function () {
    "use strict";

    var DETAIL = Object.freeze({
        OPEN: "open",
        ENTER_EDIT: "enterEdit",
        SAVE: "save",
        VALIDATE: "validate",
        AUTOSAVE: "autosave",
        CLOSE: "close",
        DELETE_CHECKLIST: "deleteChecklist",
        RESOLVE_CONFLICT: "resolveConflict",
        ATTACHMENT_UPLOAD: "attachmentUpload",
        ATTACHMENT_DELETE: "attachmentDelete",
        ATTACHMENT_LOAD: "attachmentLoad",
        ROW_OPS: "rowOps",
        VALUE_HELP_LOCATION: "valueHelpLocation",
        PERSON_SUGGEST: "personSuggest",
        DISCARD_CHANGES: "discardChanges"
    });

    var SEARCH = Object.freeze({
        BOOTSTRAP: "bootstrap",
        BUILD_FILTER: "buildFilter",
        EXECUTE_SEARCH: "executeSearch",
        REBIND: "rebind",
        SELECT_ROW: "selectRow",
        SELECTION_CHANGED: "selectionChanged",
        EXPORT_FLOW: "exportFlow",
        ANALYTICS: "analytics",
        APPLY_REBIND_POLICY: "applyRebindPolicy"
    });

    return Object.freeze({
        DETAIL: DETAIL,
        SEARCH: SEARCH
    });
});
