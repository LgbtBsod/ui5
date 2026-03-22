sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxRuntimeFactory"
], function (CtxRuntimeFactory) {
    "use strict";

    function buildDefaultCtx(oController) {
        return CtxRuntimeFactory.build(oController, {});
    }

    function buildSearchCtx(oController) {
        return CtxRuntimeFactory.build(oController, {
            smartFilterBar: oController && oController.byId && oController.byId("searchSmartFilterBar"),
            smartTable: oController && oController.byId && oController.byId("searchSmartTable")
        });
    }

    return Object.freeze({
        buildDefaultCtx: buildDefaultCtx,
        buildSearchCtx: buildSearchCtx
    });
});
