sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (CreateSentinel) {
    "use strict";

    QUnit.module("CreateSentinel");

    QUnit.test("normalizes canonical create id only", function (assert) {
        assert.strictEqual(CreateSentinel.isCreateId("__CREATE"), true, "canonical create sentinel is supported");
        assert.strictEqual(CreateSentinel.isCreateId(CreateSentinel.VALUE + "_"), false, "legacy alias is rejected");
        assert.strictEqual(CreateSentinel.isCreateId("new"), false, "legacy alias is rejected");
        assert.strictEqual(CreateSentinel.isCreateId("ROOT-100"), false, "persisted ids stay non-create");
    });
});
