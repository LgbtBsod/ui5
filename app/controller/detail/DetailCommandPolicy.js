sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput"
], function (CtxFactory, RuntimeInput) {
    "use strict";

    function normalizePayload(mInput) {
        var oInput = RuntimeInput.asObject(mInput);
        var oNormalized = Object.assign({}, oInput);
        if (Object.prototype.hasOwnProperty.call(oInput, "rootId")) {
            oNormalized.rootId = RuntimeInput.asString(oInput.rootId).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "intent")) {
            oNormalized.intent = RuntimeInput.asString(oInput.intent).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "source")) {
            oNormalized.source = RuntimeInput.asString(oInput.source).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "field")) {
            oNormalized.field = RuntimeInput.asString(oInput.field).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "key")) {
            oNormalized.key = RuntimeInput.asString(oInput.key).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "status")) {
            oNormalized.status = RuntimeInput.asString(oInput.status).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "id")) {
            oNormalized.id = RuntimeInput.asString(oInput.id).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "selectedRowId")) {
            oNormalized.selectedRowId = RuntimeInput.asString(oInput.selectedRowId).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "entity")) {
            oNormalized.entity = RuntimeInput.asString(oInput.entity).trim();
        }
        if (Object.prototype.hasOwnProperty.call(oInput, "op")) {
            oNormalized.op = RuntimeInput.asString(oInput.op).trim();
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

    function execute(oController, sMethod, mInput) {
        var oFacade = oController && oController._facade;
        var fnMethod = oFacade && oFacade[sMethod];
        if (typeof fnMethod !== "function" || !oController || typeof oController.executeFacadeMethod !== "function") {
            return Promise.resolve();
        }
        return Promise.resolve(oController.executeFacadeMethod(
            oFacade,
            sMethod,
            normalizePayload(mInput || {}),
            CtxFactory.buildCtx(oController, {})
        ));
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
