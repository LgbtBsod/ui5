sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionDispatcher"
], function (ActionDispatcher) {
    "use strict";

    QUnit.module("framework/ActionDispatcher");

    QUnit.test("dispatch forwards canonical dbKey payload without validator layer", function (assert) {
        var done = assert.async();
        var oDispatcher = new ActionDispatcher();
        var oCaptured = null;

        oDispatcher.register("detail.takeoverLock", function (mPayload) {
            oCaptured = mPayload;
            return Promise.resolve();
        });

        oDispatcher.dispatch("detail.takeoverLock", { dbKey: "CHK-00001", force: true }).then(function (bHandled) {
            assert.strictEqual(bHandled, true, "registered action is handled");
            assert.deepEqual(oCaptured, { dbKey: "CHK-00001", force: true }, "payload passes directly to the canonical handler");
            done();
        });
    });
});
