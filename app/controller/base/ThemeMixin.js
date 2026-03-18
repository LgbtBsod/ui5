sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (ThemeContracts, ThemeService, Ui5RuntimeFacade) {
    "use strict";

    var DEFAULT_MODE = ThemeContracts.MODES.MORNING;
    var DEFAULT_ANIMATION_ENABLED = ThemeService.DEFAULT_ANIMATION_ENABLED;

    function readThemeProfile() {
        var oProfile = ThemeService.getThemeProfile ? ThemeService.getThemeProfile() : null;
        return {
            mode: DEFAULT_MODE,
            animationEnabled: oProfile && typeof oProfile.animationEnabled === "boolean"
                ? oProfile.animationEnabled
                : DEFAULT_ANIMATION_ENABLED
        };
    }

    function ensureThemeSyncListener(oController) {
        if (oController._fnThemeChangedHandler) {
            return;
        }
        oController._fnThemeChangedHandler = function () {
            ThemeService.syncTokensFromUI5();
        }.bind(oController);
        Ui5RuntimeFacade.attachThemeChanged(oController._fnThemeChangedHandler);
    }

    function applyMorningMode(oController, oClickXY, mOptions) {
        var oProfile = readThemeProfile();
        ensureThemeSyncListener(oController);
        return ThemeService.applyThemeMode(DEFAULT_MODE, oClickXY || null, {
            animationEnabled: Object.prototype.hasOwnProperty.call(mOptions || {}, "animationEnabled")
                ? !!mOptions.animationEnabled
                : oProfile.animationEnabled,
            persist: !mOptions || mOptions.persist !== false
        });
    }

    return {
        getCurrentTheme: function () {
            return ThemeService.themeForMode(DEFAULT_MODE);
        },
        getCurrentThemeMode: function () {
            return DEFAULT_MODE;
        },
        isDarkThemeEnabled: function () {
            return false;
        },
        isThemeAnimationEnabled: function () {
            return !!readThemeProfile().animationEnabled;
        },
        setThemeAnimationEnabled: function (bEnabled) {
            if (ThemeService.setThemeAnimationEnabled) {
                ThemeService.setThemeAnimationEnabled(!!bEnabled);
            }
            return applyMorningMode(this, null, {
                animationEnabled: !!bEnabled,
                persist: false
            });
        },
        setThemeMode: function (_sMode, oClickXY) {
            return applyMorningMode(this, oClickXY);
        },
        applyStoredTheme: function () {
            return applyMorningMode(this, null);
        },
        toggleTheme: function (oClickXY) {
            return applyMorningMode(this, oClickXY);
        },
        _ensureThemeSyncListener: function () {
            ensureThemeSyncListener(this);
        },
        _applyTheme: function (sTheme) {
            var oProfile = readThemeProfile();
            ensureThemeSyncListener(this);
            return ThemeService.applyTheme(sTheme, null, {
                animationEnabled: oProfile.animationEnabled
            });
        }
    };
});
