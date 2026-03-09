sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result"
], function (Result) {
    "use strict";

    function ok(vData, aEffects) {
        return Promise.resolve(Result.ok(vData, aEffects || []));
    }

    function fail(vError, aEffects) {
        return Promise.resolve(Result.fail(vError, aEffects || []));
    }

    function callOrDefault(fn, vDefault) {
        if (typeof fn !== "function") {
            return Promise.resolve(vDefault);
        }
        return Promise.resolve(fn());
    }

    return {
        ok: ok,
        fail: fail,
        callOrDefault: callOrDefault
    };
});
