sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/EffectMixin"
], function (EffectMixin) {
    "use strict";

    QUnit.module("framework/EffectMixin");

    QUnit.test("executeFacadeMethod routes rejected promises through applyUseCaseEffects", function (assert) {
        var done = assert.async();
        var oExpectedError = new Error("boom");
        var oContext = {
            applyUseCaseEffects: function (oResult) {
                assert.strictEqual(oResult.ok, false, "result is normalized as failed");
                assert.strictEqual(oResult.getError(), oExpectedError, "original error is preserved");
                done();
                return Promise.resolve(oResult);
            }
        };

        EffectMixin.executeFacadeMethod.call(oContext, {
            fail: function () {
                return Promise.reject(oExpectedError);
            }
        }, "fail");
    });
});
