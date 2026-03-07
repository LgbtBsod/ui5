sap.ui.define([], function () {
    "use strict";

    function resolveCurrentRootId(oController) {
        return String((oController && oController._currentRootId && oController._currentRootId()) || "").trim();
    }

    return {
        resolveCurrentRootId: resolveCurrentRootId
    };
});
