sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ThemeMixin",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService"
], function (ThemeMixin, ThemeService) {
    "use strict";

    QUnit.module("ThemeMixin", {
        beforeEach: function () {
            this._fnApplyThemeMode = ThemeService.applyThemeMode;
            this._fnGetThemeProfile = ThemeService.getThemeProfile;
            this._fnSetThemeAnimationEnabled = ThemeService.setThemeAnimationEnabled;
        },
        afterEach: function () {
            ThemeService.applyThemeMode = this._fnApplyThemeMode;
            ThemeService.getThemeProfile = this._fnGetThemeProfile;
            ThemeService.setThemeAnimationEnabled = this._fnSetThemeAnimationEnabled;
        }
    });

    QUnit.test("defaults to morning mode", function (assert) {
        assert.strictEqual(ThemeMixin.getCurrentThemeMode(), "morning", "Morning mode remains the productive default");
        assert.strictEqual(ThemeMixin.getCurrentTheme(), "sap_fiori_3", "productive theme stays on sap_fiori_3");
    });

    QUnit.test("toggleTheme keeps the supported productive mode", function (assert) {
        var aModes = [];
        ThemeService.applyThemeMode = function (sMode) {
            aModes.push(sMode);
            return { mode: sMode };
        };

        ThemeMixin.toggleTheme();
        ThemeMixin.toggleTheme();

        assert.deepEqual(aModes, ["morning", "morning"], "toggle stays on the only supported productive mode");
    });

    QUnit.test("setThemeAnimationEnabled preserves the productive mode and updates animation state", function (assert) {
        var aCalls = [];
        ThemeService.setThemeAnimationEnabled = function (bEnabled) {
            aCalls.push({ type: "persist", animationEnabled: bEnabled });
            return { mode: "morning", animationEnabled: bEnabled };
        };
        ThemeService.applyThemeMode = function (sMode, _oClickXY, mOptions) {
            aCalls.push({
                type: "apply",
                mode: sMode,
                animationEnabled: !!(mOptions && mOptions.animationEnabled),
                persist: !!(mOptions && mOptions.persist)
            });
            return { mode: sMode, animationEnabled: !!(mOptions && mOptions.animationEnabled) };
        };

        ThemeMixin.setThemeAnimationEnabled(false);

        assert.deepEqual(aCalls, [
            { type: "persist", animationEnabled: false },
            { type: "apply", mode: "morning", animationEnabled: false, persist: false }
        ], "animation updates stay in the productive morning-only flow");
    });
});
