sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result"
], function (Result) {
    "use strict";

    function requestedInputKey(mInput) {
        return String((mInput && (mInput.rootId || mInput.id)) || "").trim();
    }

    function dbKey(mInput) {
        return String((mInput && (mInput.dbKey || mInput.DB_KEY)) || "").trim() || requestedInputKey(mInput);
    }

    function text(vValue) {
        return String(vValue || "").trim();
    }

    function bool(vValue) {
        return !!vValue;
    }

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
        bool: bool,
        callOrDefault: callOrDefault,
        dbKey: dbKey,
        fail: fail,
        ok: ok,
        requestedDbKey: requestedInputKey,
        text: text
    };
});
