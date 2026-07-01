sap.ui.define([], function () {
    "use strict";

    /* native structuredClone (Edge evergreen) preserves Date/Map/Set/typed arrays,
     * which the previous JSON.parse(JSON.stringify(...)) implementation silently
     * dropped (Date -> ISO string). A small hand-rolled recursive deep-clone is
     * the fallback for the rare environment without structuredClone — no jQuery
     * dependency is needed because the project only ever clones plain JSON model
     * data (objects/arrays/Date/primitives). */

    function deepCloneNative(vSource) {
        var vResult;
        var i;
        if (Array.isArray(vSource)) {
            vResult = [];
            for (i = 0; i < vSource.length; i += 1) {
                vResult[i] = deepCloneNative(vSource[i]);
            }
            return vResult;
        }
        if (vSource instanceof Date) {
            return new Date(vSource.getTime());
        }
        if (vSource instanceof RegExp) {
            return new RegExp(vSource.source, vSource.flags);
        }
        if (vSource instanceof Map) {
            vResult = new Map();
            vSource.forEach(function (vVal, vKey) { vResult.set(vKey, deepCloneNative(vVal)); });
            return vResult;
        }
        if (vSource instanceof Set) {
            vResult = new Set();
            vSource.forEach(function (vVal) { vResult.add(deepCloneNative(vVal)); });
            return vResult;
        }
        vResult = {};
        var aKeys = Object.keys(vSource);
        for (i = 0; i < aKeys.length; i += 1) {
            vResult[aKeys[i]] = deepCloneNative(vSource[aKeys[i]]);
        }
        return vResult;
    }

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
            /* structuredClone rejects functions/DOM nodes — fall through */
        }
        try {
            return deepCloneNative(vSource);
        } catch (_e) {
            return typeof vFallback === "undefined" ? null : vFallback;
        }
    }

    return {
        clone: clone
    };
});
