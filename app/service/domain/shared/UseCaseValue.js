sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result"
], function (Result) {
    "use strict";

    function rootId(mInput) {
        return String((mInput && (mInput.rootId || mInput.id)) || "").trim();
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
        fail: fail,
        ok: ok,
        rootId: rootId,
        text: text
    };
});
