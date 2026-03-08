sap.ui.define([], function () {
    "use strict";

    function clone(vValue, vFallback) {
        try {
            return JSON.parse(JSON.stringify(typeof vValue === "undefined" ? vFallback : vValue));
        } catch (_e) {
            return typeof vFallback === "undefined" ? null : vFallback;
        }
    }

    return {
        clone: clone
    };
});
