sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService"
], function (ThemeService) {
    "use strict";

    QUnit.module("Theme defaults");

    QUnit.test("productive theme defaults to sap_fiori_3", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("morning"), "sap_fiori_3", "Morning mode maps to SAP Fiori 3");
    });

    QUnit.test("legacy night mode tokens normalize to the productive mode", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("night"), "sap_fiori_3", "Legacy night mode safely falls back to the supported baseline");
        assert.strictEqual(ThemeService.modeForTheme("sap_fiori_3"), "morning", "Productive theme mode stays morning");
    });
});
