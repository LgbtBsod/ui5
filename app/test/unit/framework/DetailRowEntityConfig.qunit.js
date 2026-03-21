sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowEntityConfig"
], function (DetailRowEntityConfig) {
    "use strict";

    QUnit.module("framework/DetailRowEntityConfig");

    QUnit.test("returns complete check spec", function (assert) {
        var oConfig = DetailRowEntityConfig.get("check");

        assert.strictEqual(oConfig.kind, "check", "check kind is preserved");
        assert.strictEqual(oConfig.rowsPath, "/current/checks", "check rows path is defined");
        assert.strictEqual(oConfig.numberField, "ChecksNum", "check number field is defined");
        assert.strictEqual(oConfig.deleteActionKind, "check", "check delete action kind is defined");
        assert.strictEqual(oConfig.desktopVisibleRowCount, 7, "check desktop row count is defined");
        assert.strictEqual(oConfig.labelKeys.number, "checksNumberLabel", "check label keys are defined");
    });

    QUnit.test("returns complete barrier spec", function (assert) {
        var oConfig = DetailRowEntityConfig.get("barrier");

        assert.strictEqual(oConfig.kind, "barrier", "barrier kind is preserved");
        assert.strictEqual(oConfig.rowsPath, "/current/barriers", "barrier rows path is defined");
        assert.strictEqual(oConfig.numberField, "BarriersNum", "barrier number field is defined");
        assert.strictEqual(oConfig.deleteActionKind, "barrier", "barrier delete action kind is defined");
        assert.strictEqual(oConfig.desktopVisibleRowCount, 8, "barrier desktop row count is defined");
        assert.ok(oConfig.desktopTableClass.indexOf("detailBarriersGridTable") >= 0, "barrier desktop class keeps dedicated visual hook");
    });
});
