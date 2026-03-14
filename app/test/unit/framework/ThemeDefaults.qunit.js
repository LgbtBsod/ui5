sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService"
], function (ThemeService) {
    "use strict";

    QUnit.module("Theme defaults");

    QUnit.test("productive theme defaults to sap_horizon", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("morning"), "sap_horizon", "Morning mode maps to SAP Horizon");
    });

    QUnit.test("night mode mapping remains available but is not the default productive mode", function (assert) {
        assert.strictEqual(ThemeService.themeForMode("night"), "sap_horizon_dark", "Night mode still maps correctly");
        assert.strictEqual(ThemeService.modeForTheme("sap_horizon"), "morning", "Productive theme mode stays morning");
    });
});
