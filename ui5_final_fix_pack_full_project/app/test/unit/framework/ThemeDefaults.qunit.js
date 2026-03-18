sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService"
], function (ThemeService) {
    "use strict";

    QUnit.module("Theme defaults");

    QUnit.test("productive theme defaults to sap_fiori_3", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("morning"), "sap_fiori_3", "Morning mode maps to SAP Fiori 3");
    });

    QUnit.test("night mode is normalized to the safe morning theme for UI5 1.71", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("night"), "sap_fiori_3", "Night mode falls back to SAP Fiori 3");
        assert.strictEqual(ThemeService.modeForTheme("sap_horizon"), "morning", "Legacy Horizon theme is normalized to morning mode");
    });
});
