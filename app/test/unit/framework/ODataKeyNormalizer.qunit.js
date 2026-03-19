sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer"
], function (ODataKeyNormalizer) {
    "use strict";

    QUnit.module("framework/ODataKeyNormalizer");

    QUnit.test("normalizes binary keys for OData paths and filters", function (assert) {
        assert.strictEqual(
            ODataKeyNormalizer.normalizeBinaryKey("550e8400-e29b-41d4-a716-446655440000"),
            "550E8400E29B41D4A716446655440000",
            "hyphens are removed and casing is normalized"
        );
    });
});
