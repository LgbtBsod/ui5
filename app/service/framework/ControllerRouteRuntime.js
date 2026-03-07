sap.ui.define([], function () {
    "use strict";

    function attachMatched(oController, aRoutes) {
        if (!oController || typeof oController.attachRouteMatched !== "function" || !Array.isArray(aRoutes)) {
            return 0;
        }
        aRoutes.forEach(function (oEntry) {
            if (!oEntry || !oEntry.name || typeof oEntry.handler !== "function") {
                return;
            }
            oController.attachRouteMatched(String(oEntry.name), oEntry.handler);
        });
        return aRoutes.length;
    }

    function detachAllMatched(oController) {
        if (!oController || typeof oController.detachAllRouteMatched !== "function") {
            return false;
        }
        oController.detachAllRouteMatched();
        return true;
    }

    return {
        attachMatched: attachMatched,
        detachAllMatched: detachAllMatched
    };
});
