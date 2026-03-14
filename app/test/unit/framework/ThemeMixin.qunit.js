sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ThemeMixin"
], function (ThemeMixin) {
    "use strict";

    QUnit.module("ThemeMixin", {
        beforeEach: function () {
            try {
                window.localStorage.removeItem("checklist_app_theme_dev_override");
            } catch (_error) {
                // Best-effort cleanup only.
            }
        },
        afterEach: function () {
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
        window.localStorage.setItem("checklist_app_theme_dev_override", "night");

        assert.strictEqual(ThemeMixin.getCurrentThemeMode(), "night", "Night mode can be enabled through dev override");
        assert.strictEqual(ThemeMixin.isDarkThemeEnabled(), true, "Dark mode reports enabled when overridden");
    });
