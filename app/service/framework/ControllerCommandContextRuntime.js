sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory"
], function (CtxFactory) {
    "use strict";

    function buildDefaultCtx(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    function buildSearchCtx(oController) {
        return CtxFactory.buildCtx(oController, {
            smartFilterBar: oController && oController.byId && oController.byId("searchSmartFilterBar"),
            smartTable: oController && oController.byId && oController.byId("searchSmartTable")
        });
    }

    return Object.freeze({
        buildDefaultCtx: buildDefaultCtx,
        buildSearchCtx: buildSearchCtx
    });
});
