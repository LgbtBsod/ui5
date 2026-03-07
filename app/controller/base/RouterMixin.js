sap.ui.define([], function () {
    "use strict";

    function resolveRouter(oController) {
        var oOwnerComponent = oController && oController.getOwnerComponent && oController.getOwnerComponent();
        return oOwnerComponent && oOwnerComponent.getRouter ? oOwnerComponent.getRouter() : null;
    }

    function routeHooks(oController) {
        if (!oController._aRouteMatchedHooks) {
            oController._aRouteMatchedHooks = [];
        }
        return oController._aRouteMatchedHooks;
    }

    function sameHook(oHook, sRouteName, fnHandler) {
        return oHook && oHook.routeName === sRouteName && oHook.fnHandler === fnHandler;
    }

    return {
        getRouter: function () { return resolveRouter(this); },
        navTo: function (sRoute, oParameters, bReplace) {
            var oRouter = this.getRouter();
            if (oRouter && oRouter.navTo) {
                oRouter.navTo(sRoute, oParameters || {}, bReplace);
            }
        },
        attachRouteMatched: function (sRouteName, fnHandler) {
            var oRouter = this.getRouter();
            var oRoute = oRouter && oRouter.getRoute ? oRouter.getRoute(sRouteName) : null;
            var aHooks = routeHooks(this);
            if (!oRoute || !oRoute.attachPatternMatched || typeof fnHandler !== "function") {
                return null;
            }
            if (!aHooks.some(function (oHook) { return sameHook(oHook, sRouteName, fnHandler); })) {
                oRoute.attachPatternMatched(fnHandler, this);
                aHooks.push({
                    routeName: sRouteName,
                    route: oRoute,
                    fnHandler: fnHandler
                });
            }
            return oRoute;
        },
        detachRouteMatched: function (sRouteName, fnHandler) {
            var aHooks = routeHooks(this);
            this._aRouteMatchedHooks = aHooks.filter(function (oHook) {
                var bMatch = sameHook(oHook, sRouteName, fnHandler);
                if (bMatch && oHook.route && oHook.route.detachPatternMatched) {
                    oHook.route.detachPatternMatched(oHook.fnHandler, this);
                }
                return !bMatch;
            }, this);
        },
        detachAllRouteMatched: function () {
            routeHooks(this).forEach(function (oHook) {
                if (oHook && oHook.route && oHook.route.detachPatternMatched) {
                    oHook.route.detachPatternMatched(oHook.fnHandler, this);
                }
            }, this);
            this._aRouteMatchedHooks = [];
        }
    };
});
