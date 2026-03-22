sap.ui.define([], function () {
    "use strict";

    function resolveTimezone() {
        try {
            return Intl.DateTimeFormat().resolvedOptions().timeZone || "UTC";
        } catch (_error) {
            return "UTC";
        }
    }

    return Object.freeze({
        resolveTimezone: resolveTimezone
    });
});
