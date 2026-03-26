sap.ui.define([], function () {
    "use strict";

    function getRouter(oController) {
        return oController && oController.getRouter ? oController.getRouter() : null;
    }

    function getRegistry(oController) {
        oController._routeRuntimeRegistry = oController._routeRuntimeRegistry || [];
        return oController._routeRuntimeRegistry;
    }

    function attachMatched(oController, aRoutes) {
        var oRouter = getRouter(oController);
        if (!oRouter || !Array.isArray(aRoutes)) {
            return [];
        }
        aRoutes.forEach(function (oRouteConfig) {
            var sName = String(oRouteConfig && oRouteConfig.name || "").trim();
            var fnHandler = oRouteConfig && oRouteConfig.handler;
            var oRoute;
            if (!sName || typeof fnHandler !== "function" || !oRouter.getRoute) {
                return;
            }
            oRoute = oRouter.getRoute(sName);
            if (!oRoute || !oRoute.attachPatternMatched) {
                return;
            }
            oRoute.attachPatternMatched(fnHandler, oController);
            getRegistry(oController).push({
                route: oRoute,
                handler: fnHandler
            });
        });
        return getRegistry(oController);
    }

    function detachAllMatched(oController) {
        getRegistry(oController).forEach(function (oEntry) {
            if (oEntry.route && oEntry.route.detachPatternMatched) {
                oEntry.route.detachPatternMatched(oEntry.handler, oController);
            }
        });
        oController._routeRuntimeRegistry = [];
    }

    return Object.freeze({
        attachMatched: attachMatched,
        detachAllMatched: detachAllMatched
    });
});
