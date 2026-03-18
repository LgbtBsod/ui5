sap.ui.define([], function () {
    "use strict";

    function resolveCallback(fnCallback) {
        return typeof fnCallback === "function" ? fnCallback() : undefined;
    }

    function withFinally(vPromise, fnCallback) {
        return Promise.resolve(vPromise).then(function (vValue) {
            return Promise.resolve(resolveCallback(fnCallback)).then(function () {
                return vValue;
            });
        }, function (oError) {
            return Promise.resolve(resolveCallback(fnCallback)).then(function () {
                throw oError;
            });
        });
    }

    return {
        withFinally: withFinally
    };
});
