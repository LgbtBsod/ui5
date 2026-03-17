sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService"
], function (ThemeService) {
    "use strict";

    QUnit.module("Theme defaults");

    QUnit.test("productive theme defaults to sap_fiori_3", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("morning"), "sap_fiori_3", "Morning mode maps to SAP Fiori 3");
    });

    QUnit.test("night mode mapping remains available but is not the default productive mode", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("night"), "sap_fiori_3_dark", "Night mode maps correctly");
        assert.strictEqual(ThemeService.modeForTheme("sap_fiori_3"), "morning", "Productive theme mode stays morning");
    });
});
