sap.ui.define([], function () {
    "use strict";

    function toValidationKey(sPath) {
        return String(sPath || "").replace(/^\//, "").replace(/\//g, ".");
    }

    function toMissingMap(aMissingPaths) {
        return (Array.isArray(aMissingPaths) ? aMissingPaths : []).reduce(function (mAcc, sPath) {
            mAcc[toValidationKey(sPath)] = true;
            return mAcc;
        }, {});
    }

    return {
        toMissingMap: toMissingMap,
        toValidationKey: toValidationKey
    };
});
