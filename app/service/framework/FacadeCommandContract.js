sap.ui.define([
    "checklist/app/service/framework/RuntimeInput"
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
        CHANGE_STATUS: "changeStatus",
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

    function applyCommonPayloadNormalization(oPayload) {
        var oInput = RuntimeInput.asObject(oPayload);
        var oNormalized = Object.assign({}, oInput);
        if (Object.prototype.hasOwnProperty.call(oInput, "rootId")) {
            oNormalized.rootId = RuntimeInput.asString(oInput.rootId).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "selectedRowId")) {
            oNormalized.selectedRowId = RuntimeInput.asString(oInput.selectedRowId).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "intent")) {
            oNormalized.intent = RuntimeInput.asString(oInput.intent).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "source")) {
            oNormalized.source = RuntimeInput.asString(oInput.source).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "entity")) {
            oNormalized.entity = RuntimeInput.asString(oInput.entity).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "op")) {
            oNormalized.op = RuntimeInput.asString(oInput.op).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "field")) {
            oNormalized.field = RuntimeInput.asString(oInput.field).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "key")) {
            oNormalized.key = RuntimeInput.asString(oInput.key).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "state")) {
            oNormalized.state = RuntimeInput.asBoolean(oInput.state, false);
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "silent")) {
            oNormalized.silent = RuntimeInput.asBoolean(oInput.silent, false);
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "userInitiated")) {
            oNormalized.userInitiated = RuntimeInput.asBoolean(oInput.userInitiated, false);
        }
        return oNormalized;
    }

    function normalizeDetailMethod(vMethod) {
        return normalizeKnownMethod(vMethod, DETAIL_METHODS);
    }

    function normalizeSearchMethod(vMethod) {
        return normalizeKnownMethod(vMethod, SEARCH_METHODS);
    }

    function normalizeDetailPayload(_sMethod, oPayload) {
        return applyCommonPayloadNormalization(oPayload);
    }

    function normalizeSearchPayload(_sMethod, oPayload) {
        return applyCommonPayloadNormalization(oPayload);
    }

    return {
        DETAIL_METHODS: DETAIL_METHODS,
        SEARCH_METHODS: SEARCH_METHODS,
        normalizeDetailMethod: normalizeDetailMethod,
        normalizeSearchMethod: normalizeSearchMethod,
        normalizeDetailPayload: normalizeDetailPayload,
        normalizeSearchPayload: normalizeSearchPayload
    };
});
