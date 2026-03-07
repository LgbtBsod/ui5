sap.ui.define([], function () {
    "use strict";

    function create() {
        return {
            now: function () {
                return Date.now();
            },
            isoNow: function () {
                return new Date().toISOString();
            }
        };
    }

    return {
        create: create
    };
});
