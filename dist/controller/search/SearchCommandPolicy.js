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

    function buildSearchCtx(oController) {
        return CtxFactory.buildCtx(oController, {
            smartFilterBar: oController && oController.byId && oController.byId("searchSmartFilterBar"),
            smartTable: oController && oController.byId && oController.byId("searchSmartTable")
        });
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
            buildSearchCtx(oController)
        ));
    }

    return Object.freeze({
        buildFilter: function (oController, mInput) {
            return execute(oController, "buildFilter", mInput);
        },
        executeSearch: function (oController, mInput) {
            return execute(oController, "executeSearch", mInput);
        },
        rebind: function (oController, mInput) {
            return execute(oController, "rebind", mInput);
        },
        selectRow: function (oController, mInput) {
            return execute(oController, "selectRow", mInput);
        },
        selectionChanged: function (oController, mInput) {
            return execute(oController, "selectionChanged", mInput);
        },
        bootstrap: function (oController, mInput) {
            return execute(oController, "bootstrap", mInput);
        },
        analytics: function (oController, mInput) {
            return execute(oController, "analytics", mInput);
        },
        applyRebindPolicy: function (oController, mInput) {
            return execute(oController, "applyRebindPolicy", mInput);
        },
        exportFlow: function (oController, mInput) {
            return execute(oController, "exportFlow", mInput);
        }
    });
});
