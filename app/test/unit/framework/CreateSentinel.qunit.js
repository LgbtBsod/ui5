sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (CreateSentinel) {
    "use strict";

    QUnit.module("CreateSentinel");

    QUnit.test("normalizes current and legacy create ids", function (assert) {
        assert.strictEqual(CreateSentinel.isCreateId("__CREATE"), true, "canonical create sentinel is supported");
        assert.strictEqual(CreateSentinel.isCreateId(" __CREATE__ "), true, "trimmed legacy create sentinel is supported");
        assert.strictEqual(CreateSentinel.isCreateId("new"), true, "legacy create sentinel is still recognized");
        assert.strictEqual(CreateSentinel.isCreateId("ROOT-100"), false, "persisted ids stay non-create");
    });
});
