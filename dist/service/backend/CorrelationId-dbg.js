sap.ui.define([], function () {
    "use strict";

    function next(sPrefix) {
        var sSafePrefix = String(sPrefix || "req").trim() || "req";
        return [
            sSafePrefix,
            Date.now().toString(36),
            Math.random().toString(36).slice(2, 10)
        ].join("-");
    }

    return {
        next: next
    };
});
