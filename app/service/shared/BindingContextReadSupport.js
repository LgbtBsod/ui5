sap.ui.define([], function () {
    "use strict";

    function read(oContext, sPath, vFallback) {
        if (!oContext || typeof oContext.getProperty !== "function") {
            return vFallback;
        }
        return oContext.getProperty(sPath);
    }

    function readAny(oContext, aPaths, vFallback) {
        var i;
        var vValue;
        if (!oContext || !Array.isArray(aPaths) || !aPaths.length || typeof oContext.getProperty !== "function") {
            return vFallback;
        }
        for (i = 0; i < aPaths.length; i += 1) {
            vValue = oContext.getProperty(aPaths[i]);
            if (vValue !== undefined && vValue !== null && vValue !== "") {
                return vValue;
            }
        }
        return vFallback;
    }

    return {
        read: read,
        readAny: readAny
    };
});
