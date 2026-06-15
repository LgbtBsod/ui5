sap.ui.define([
    "sap/ui/thirdparty/jquery"
], function (jQuery) {
    "use strict";

    /* native structuredClone (Edge evergreen) preserves Date/Map/Set/typed arrays,
     * which the previous JSON.parse(JSON.stringify(...)) implementation silently
     * dropped (Date -> ISO string). jQuery.extend(true, ...) is the UI5-shipped
     * fallback for the rare environment without structuredClone and for any input
     * structuredClone rejects (functions/DOM nodes). */
    function clone(vValue, vFallback) {
        var vSource = typeof vValue === "undefined" ? vFallback : vValue;
        if (vSource === null || typeof vSource !== "object") {
            return vSource;
        }
        try {
            if (typeof structuredClone === "function") {
                return structuredClone(vSource);
            }
        } catch (_structuredCloneError) {
            /* fall through to jQuery.extend */
        }
        try {
            return jQuery.extend(true, Array.isArray(vSource) ? [] : {}, vSource);
        } catch (_e) {
            return typeof vFallback === "undefined" ? null : vFallback;
        }
    }

    return {
        clone: clone
    };
});
