sap.ui.define([], function () {
    "use strict";

    function writeText(sValue) {
        var sText = String(sValue || "");
        if (!sText || !navigator || !navigator.clipboard || typeof navigator.clipboard.writeText !== "function") {
            return Promise.resolve(false);
        }
        return navigator.clipboard.writeText(sText).then(function () {
            return true;
        }).catch(function () {
            return false;
        });
    }

    return {
        writeText: writeText
    };
});
