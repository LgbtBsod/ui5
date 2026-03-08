sap.ui.define(["checklist/app/service/framework/FeedbackPolicy"], function (FeedbackPolicy) {
    "use strict";

    function normalizeEffects(aEffects) {
        return Array.isArray(aEffects) ? aEffects : [];
    }

    function ok(vData, aEffects) {
        return {
            ok: true,
            data: vData,
            effects: normalizeEffects(aEffects)
        };
    }

    function fail(vError, aEffects) {
        var oError = FeedbackPolicy.normalize(vError);
        return {
            ok: false,
            error: oError,
            effects: normalizeEffects(aEffects && aEffects.length ? aEffects : FeedbackPolicy.toEffects(vError))
        };
    }

    return {
        ok: ok,
        fail: fail
    };
});
