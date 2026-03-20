sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput"
], function (RuntimeInput) {
    "use strict";

    var DETAIL_METHODS = {
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
        ROW_OPS: "rowOps",
        VALUE_HELP_LOCATION: "valueHelpLocation",
        PERSON_SUGGEST: "personSuggest",
        DISCARD_CHANGES: "discardChanges"
    };

    var SEARCH_METHODS = {
        BOOTSTRAP: "bootstrap",
        BUILD_FILTER: "buildFilter",
        EXECUTE_SEARCH: "executeSearch",
        REBIND: "rebind",
        SELECT_ROW: "selectRow",
        SELECTION_CHANGED: "selectionChanged",
        EXPORT_FLOW: "exportFlow",
        ANALYTICS: "analytics",
        APPLY_REBIND_POLICY: "applyRebindPolicy"
    };

    function normalizeKnownMethod(vMethod, mKnown) {
        var sMethod = RuntimeInput.asString(vMethod).trim();
        if (!sMethod) {
            return "";
        }
        return Object.keys(mKnown).some(function (sKey) {
            return mKnown[sKey] === sMethod;
        }) ? sMethod : sMethod;
    }

    return {
        DETAIL_METHODS: DETAIL_METHODS,
        SEARCH_METHODS: SEARCH_METHODS,
        normalizeKnownMethod: normalizeKnownMethod
    };
});
