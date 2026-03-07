sap.ui.define([], function () {
    "use strict";

    function asObject(vValue) {
        return vValue && typeof vValue === "object" && !Array.isArray(vValue) ? vValue : {};
    }

    function asString(vValue, sFallback) {
        var sBase = typeof sFallback === "string" ? sFallback : "";
        if (vValue === null || typeof vValue === "undefined") {
            return sBase;
        }
        return String(vValue);
    }

    function asBoolean(vValue, bFallback) {
        if (typeof vValue === "boolean") {
            return vValue;
        }
        return !!bFallback;
    }

    return {
        asObject: asObject,
        asString: asString,
        asBoolean: asBoolean
    };
});
