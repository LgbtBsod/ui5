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

    QUnit.test("normalizes persisted dev override to morning mode", function (assert) {
        window.__DEBUG_UI5__ = true;
        window.localStorage.setItem("checklist_app_theme_dev_override", "night");

        assert.strictEqual(ThemeMixin.getCurrentThemeMode(), "morning", "Legacy night override is normalized to the productive morning mode");
        assert.strictEqual(ThemeMixin.isDarkThemeEnabled(), false, "Dark mode stays disabled after normalization");
    });

    QUnit.test("toggleTheme keeps the supported productive mode", function (assert) {
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

        assert.deepEqual(aModes, ["morning", "morning"], "toggle stays on the only supported productive mode");
    });
});
