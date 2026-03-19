sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackPolicy"], function (FeedbackPolicy) {
    "use strict";

    function normalizeEffects(aEffects) {
        return Array.isArray(aEffects) ? aEffects : [];
    }

    function ok(vData, aEffects) {
        return {
            ok: true,
            data: vData,
            effects: normalizeEffects(aEffects),
            isOk: function () {
                return true;
            },
            getValue: function () {
                return vData;
            },
            getError: function () {
                return null;
            }
        };
    }

    function fail(vError, aEffects) {
        var oError = FeedbackPolicy.normalize(vError);
        return {
            ok: false,
            error: oError,
            effects: normalizeEffects(aEffects && aEffects.length ? aEffects : FeedbackPolicy.toEffects(vError)),
            isOk: function () {
                return false;
            },
            getValue: function () {
                return null;
            },
            getError: function () {
                return oError;
            }
        };
    }

    return {
        ok: ok,
        fail: fail
    };
});
