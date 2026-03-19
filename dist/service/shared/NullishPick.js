sap.ui.define([], function () {
    "use strict";

    function firstDefined() {
        var iIndex;
        for (iIndex = 0; iIndex < arguments.length; iIndex += 1) {
            if (arguments[iIndex] !== undefined && arguments[iIndex] !== null) {
                return arguments[iIndex];
            }
        }
        return undefined;
    }

    return Object.freeze({
        firstDefined: firstDefined
    });
});
