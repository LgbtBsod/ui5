sap.ui.define([], function () {
    "use strict";

    function normalizeBinaryKey(sValue) {
        return String(sValue || "").replace(/-/g, "").trim().toUpperCase();
    }

    return {
        normalizeBinaryKey: normalizeBinaryKey
    };
});
