sap.ui.define([
    "sap/ui/core/routing/HashChanger",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteHashResolver"
], function (HashChanger, RouteHashResolver) {
    "use strict";

    /* route.attachPatternMatched()/router.attachRoutePatternMatched() do not reliably reach
     * app-level listeners for in-app navTo() transitions in this build (confirmed: FlexibleColumnLayout
     * column switching is driven by the router's private target/layout wiring, not by this public
     * event). HashChanger's hashChanged event is the lower-level, framework-guaranteed signal every
     * navigation goes through, so every route-matched consumer is anchored there instead. */

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
            var oController = this;
            var aHooks = routeHooks(this);
            var fnHashChanged;
            if (typeof fnHandler !== "function" || aHooks.some(function (oHook) { return sameHook(oHook, sRouteName, fnHandler); })) {
                return null;
            }
            fnHashChanged = function () {
                var oRoute = RouteHashResolver.resolveRouteFromHash(HashChanger.getInstance().getHash());
                if (oRoute.name === sRouteName) {
                    fnHandler.call(oController, RouteHashResolver.buildRouteEvent(oRoute));
                }
            };
            HashChanger.getInstance().attachEvent("hashChanged", fnHashChanged);
            aHooks.push({
                routeName: sRouteName,
                fnHandler: fnHandler,
                fnHashChanged: fnHashChanged
            });
            /* A view created lazily in reaction to a hash change (e.g. an FCL column instantiated by
             * the very navigation this hook wants to observe) attaches its listener after that
             * hashChanged event already dispatched to the listeners registered at the time - so it
             * silently misses the navigation that caused its own creation. Re-checking the current
             * hash once on a microtask (after the attaching controller's synchronous init finishes)
             * catches that race without relying on view re-render timing. */
            Promise.resolve().then(fnHashChanged);
            return true;
        },
        detachRouteMatched: function (sRouteName, fnHandler) {
            var aHooks = routeHooks(this);
            this._aRouteMatchedHooks = aHooks.filter(function (oHook) {
                var bMatch = sameHook(oHook, sRouteName, fnHandler);
                if (bMatch && oHook.fnHashChanged) {
                    HashChanger.getInstance().detachEvent("hashChanged", oHook.fnHashChanged);
                }
                return !bMatch;
            }, this);
        },
        detachAllRouteMatched: function () {
            routeHooks(this).forEach(function (oHook) {
                if (oHook && oHook.fnHashChanged) {
                    HashChanger.getInstance().detachEvent("hashChanged", oHook.fnHashChanged);
                }
            }, this);
            this._aRouteMatchedHooks = [];
        }
    };
});
