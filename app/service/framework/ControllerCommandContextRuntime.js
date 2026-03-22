sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxRuntimeFactory",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds"
], function (CtxRuntimeFactory, UiControlIds) {
    "use strict";

    function buildDefaultCtx(oController) {
        return CtxRuntimeFactory.build(oController, {});
    }

    function buildSearchCtx(oController) {
        return CtxRuntimeFactory.build(oController, {
            smartFilterBar: oController && oController.byId && oController.byId(UiControlIds.SEARCH.SMART_FILTER_BAR),
            smartTable: oController && oController.byId && oController.byId(UiControlIds.SEARCH.SMART_TABLE)
        });
    }

    return Object.freeze({
        buildDefaultCtx: buildDefaultCtx,
        buildSearchCtx: buildSearchCtx
    });
});
