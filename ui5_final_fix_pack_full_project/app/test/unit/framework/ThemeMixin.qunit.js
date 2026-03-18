sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ThemeMixin",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService"
], function (ThemeMixin, ThemeService) {
    "use strict";

    QUnit.module("ThemeMixin", {
        beforeEach: function () {
            this._fnApplyThemeMode = ThemeService.applyThemeMode;
            this._bDebugUi5 = window.__DEBUG_UI5__;
            try {
                window.localStorage.removeItem("checklist_app_theme_dev_override");
            } catch (_error) {
                // Best-effort cleanup only.
            }
        },
        afterEach: function () {
            ThemeService.applyThemeMode = this._fnApplyThemeMode;
            window.__DEBUG_UI5__ = this._bDebugUi5;
            try {
                window.localStorage.removeItem("checklist_app_theme_dev_override");
            } catch (_error) {
                // Best-effort cleanup only.
            }
        }
    });

    QUnit.test("defaults to morning mode", function (assert) {
        assert.strictEqual(ThemeMixin.getCurrentThemeMode(), "morning", "Morning mode remains the productive default");
        assert.strictEqual(ThemeMixin.isDarkThemeEnabled(), false, "Dark mode is disabled by default");
    });

    QUnit.test("supports persisted dev override for night mode", function (assert) {
        window.__DEBUG_UI5__ = true;
        window.localStorage.setItem("checklist_app_theme_dev_override", "night");

        assert.strictEqual(ThemeMixin.getCurrentThemeMode(), "night", "Night mode can be enabled through dev override");
        assert.strictEqual(ThemeMixin.isDarkThemeEnabled(), true, "Dark mode reports enabled when overridden");
    });

    QUnit.test("toggleTheme switches between supported modes", function (assert) {
        var aModes = [];
        window.__DEBUG_UI5__ = true;
        ThemeService.applyThemeMode = function (sMode) {
            aModes.push(sMode);
            return { mode: sMode };
        };

        window.localStorage.setItem("checklist_app_theme_dev_override", "morning");
        ThemeMixin.toggleTheme();
        window.localStorage.setItem("checklist_app_theme_dev_override", "night");
        ThemeMixin.toggleTheme();

        assert.deepEqual(aModes, ["night", "morning"], "toggle resolves the next real theme mode");
    });
});
