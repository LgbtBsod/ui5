sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var REGISTRY_KEY = "__controllerRouteRuntimeEntries";

    function routerFor(oController) {
        return oController && typeof oController.getRouter === TYPE_FUNCTION ? oController.getRouter() : null;
    }

    function targetHandler(oController, vHandler) {
        if (typeof vHandler === TYPE_FUNCTION) {
            return vHandler.bind(oController);
        }
        if (oController && typeof oController[vHandler] === TYPE_FUNCTION) {
            return oController[vHandler].bind(oController);
        }
        return null;
    }

    function attachMatched(oController, aEntries) {
        var oRouter = routerFor(oController);
        if (!oRouter || !Array.isArray(aEntries)) {
            return [];
        }
        oController[REGISTRY_KEY] = oController[REGISTRY_KEY] || [];
        aEntries.forEach(function (oEntry) {
            var sName = String(oEntry && oEntry.name || "").trim();
            var fnHandler = targetHandler(oController, oEntry && oEntry.handler);
            var oRoute = sName && typeof oRouter.getRoute === TYPE_FUNCTION ? oRouter.getRoute(sName) : null;
            if (!oRoute || typeof oRoute.attachMatched !== TYPE_FUNCTION || !fnHandler) {
                return;
            }
            oRoute.attachMatched(fnHandler);
            oController[REGISTRY_KEY].push({ route: oRoute, handler: fnHandler });
        });
        return oController[REGISTRY_KEY];
    }

    function detachAllMatched(oController) {
        var aEntries = oController && oController[REGISTRY_KEY];
        if (!Array.isArray(aEntries)) {
            return;
        }
        aEntries.forEach(function (oEntry) {
            if (oEntry && oEntry.route && typeof oEntry.route.detachMatched === TYPE_FUNCTION && oEntry.handler) {
                oEntry.route.detachMatched(oEntry.handler);
            }
        });
        oController[REGISTRY_KEY] = [];
    }

    return Object.freeze({
        attachMatched: attachMatched,
        detachAllMatched: detachAllMatched
    });
});
