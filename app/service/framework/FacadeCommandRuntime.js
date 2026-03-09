sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput"
], function (CtxFactory, RuntimeInput) {
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

    function normalizePayload(oPayload) {
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

    function buildDefaultCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    function buildSearchCtx(oController) {
        return CtxFactory.buildCtx(oController, {
            smartFilterBar: oController && oController.byId && oController.byId("searchSmartFilterBar"),
            smartTable: oController && oController.byId && oController.byId("searchSmartTable")
        });
    }

    function executeRaw(oController, oFacade, sMethod, mInput, mCtx) {
        var fn = oFacade && oFacade[sMethod];
        if (typeof fn !== "function") {
            return Promise.resolve();
        }
        return Promise.resolve(oController.executeFacadeMethod(oFacade, sMethod, mInput || {}, mCtx || {}));
    }

    function executeWithContract(oController, oFacade, sMethod, mInput, mCtx, mContract) {
        var sCommand = mContract && typeof mContract.normalizeMethod === "function"
            ? mContract.normalizeMethod(sMethod)
            : sMethod;
        var oPayload = mContract && typeof mContract.normalizePayload === "function"
            ? mContract.normalizePayload(sCommand, mInput)
            : (mInput || {});
        return executeRaw(oController, oFacade, sCommand, oPayload, mCtx);
    }

    function executeNamed(oController, oFacade, sMethod, mInput, mProfile) {
        var oProfile = mProfile || {};
        var fnBuildCtx = typeof oProfile.buildCtx === "function"
            ? oProfile.buildCtx
            : buildDefaultCtx;
        return executeWithContract(
            oController,
            oFacade,
            sMethod,
            mInput || {},
            fnBuildCtx(oController),
            {
                normalizeMethod: oProfile.normalizeMethod,
                normalizePayload: oProfile.normalizePayload
            }
        );
    }

    function executeDetail(oController, oFacade, sMethod, mInput) {
        return executeNamed(oController, oFacade, sMethod, mInput, {
            buildCtx: buildDefaultCtx,
            normalizeMethod: function (vMethod) {
                return normalizeKnownMethod(vMethod, DETAIL_METHODS);
            },
            normalizePayload: normalizePayload
        });
    }

    function executeSearch(oController, oFacade, sMethod, mInput) {
        return executeNamed(oController, oFacade, sMethod, mInput, {
            buildCtx: buildSearchCtx,
            normalizeMethod: function (vMethod) {
                return normalizeKnownMethod(vMethod, SEARCH_METHODS);
            },
            normalizePayload: normalizePayload
        });
    }

    return {
        executeRaw: executeRaw,
        executeWithContract: executeWithContract,
        executeNamed: executeNamed,
        executeDetail: executeDetail,
        executeSearch: executeSearch
    };
});
