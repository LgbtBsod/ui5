sap.ui.define([], function () {
    "use strict";

    var TRUE_TOKENS = {
        true: true,
        "1": true,
        on: true
    };

    var FALSE_TOKENS = {
        false: true,
        "0": true,
        off: true
    };

    function parseBooleanToken(vValue, bFallback) {
        var sToken;
        if (typeof vValue === "boolean") {
            return vValue;
        }
        if (typeof vValue === "number") {
            return vValue !== 0;
        }
        if (typeof vValue !== "string") {
            return bFallback;
        }
        sToken = vValue.trim().toLowerCase();
        if (TRUE_TOKENS[sToken]) {
            return true;
        }
        if (FALSE_TOKENS[sToken]) {
            return false;
        }
        return bFallback;
    }

    return {
        parseBooleanToken: parseBooleanToken
    };
});
