sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts"
], function (NavigationContracts) {
    "use strict";

    /* sap.f.routing.Router's public routePatternMatched / route.attachPatternMatched events do not
     * reliably reach app-level listeners for in-app navTo() transitions in this build (confirmed:
     * the FlexibleColumnLayout column switch itself is driven by the router's private target/layout
     * wiring, not by this public event). sap.ui.core.routing.HashChanger's hashChanged event is the
     * lower-level, framework-guaranteed signal every navigation goes through, so every app-level
     * route-matched consumer is anchored there instead of on the higher-level event that never fires.
     * Single source of truth for hash -> {name, args} parsing, shared by RouteModeCoordinator and
     * RouterMixin so every consumer agrees on the same route shape. */
    var DETAIL_HASH_PATTERN = /^checklist\/(.+)$/;

    function resolveRouteFromHash(sHash) {
        var sNormalized = String(sHash || "").replace(/^#/, "").replace(/^\//, "");
        var oMatch;
        if (!sNormalized) {
            return { name: NavigationContracts.ROUTES.SEARCH, args: {} };
        }
        if (sNormalized === NavigationContracts.ROUTES.ANALYTICS) {
            return { name: NavigationContracts.ROUTES.ANALYTICS, args: {} };
        }
        oMatch = DETAIL_HASH_PATTERN.exec(sNormalized);
        if (oMatch) {
            return { name: NavigationContracts.ROUTES.DETAIL, args: { id: decodeURIComponent(oMatch[1]) } };
        }
        return { name: NavigationContracts.ROUTES.SEARCH, args: {} };
    }

    function buildRouteEvent(oRoute) {
        return {
            getParameter: function (sParam) {
                if (sParam === "name") {
                    return oRoute.name;
                }
                if (sParam === "arguments") {
                    return oRoute.args;
                }
                return undefined;
            }
        };
    }

    return {
        resolveRouteFromHash: resolveRouteFromHash,
        buildRouteEvent: buildRouteEvent
    };
});
