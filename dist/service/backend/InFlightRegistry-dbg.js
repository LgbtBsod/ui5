sap.ui.define([], function () {
    "use strict";

    var mEntries = {};

    function normalizeKey(vKey) {
        return String(vKey || "").trim();
    }

    function get(vKey) {
        return mEntries[normalizeKey(vKey)] || null;
    }

    function set(vKey, oEntry) {
        var sKey = normalizeKey(vKey);
        if (!sKey) {
            return oEntry || null;
        }
        mEntries[sKey] = oEntry || null;
        return mEntries[sKey];
    }

    function remove(vKey, oEntry) {
        var sKey = normalizeKey(vKey);
        if (!sKey) {
            return;
        }
        if (typeof oEntry === "undefined" || mEntries[sKey] === oEntry) {
            delete mEntries[sKey];
        }
    }

    function clear() {
        mEntries = {};
    }

    return {
        get: get,
        set: set,
        remove: remove,
        clear: clear
    };
});
