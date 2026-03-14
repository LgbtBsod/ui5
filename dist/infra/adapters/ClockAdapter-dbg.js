sap.ui.define([], function () {
    "use strict";

    function now() {
        return Date.now();
    }

    function isoNow() {
        return new Date().toISOString();
    }

    return {
        isoNow: isoNow,
        now: now
    };
});
