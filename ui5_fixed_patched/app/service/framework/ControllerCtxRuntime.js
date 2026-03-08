sap.ui.define([
    "checklist/app/service/framework/CtxFactory"
], function (CtxFactory) {
    "use strict";

    function buildDefault(oController) {
        return CtxFactory.buildCtx(oController, {});
    }

    function buildSearch(oController) {
        return CtxFactory.buildCtx(oController, {
            smartFilterBar: oController && oController.byId && oController.byId("searchSmartFilterBar"),
            smartTable: oController && oController.byId && oController.byId("searchSmartTable")
        });
    }

    return {
        buildDefault: buildDefault,
        buildSearch: buildSearch
    };
});
