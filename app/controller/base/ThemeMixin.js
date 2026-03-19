sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (ThemeContracts, ThemeService, Ui5RuntimeFacade) {
    "use strict";

    // Productive mode is currently locked to morning/light.
    // Keep the public API intact because app shell and tests depend on it.
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

    function applySupportedMode(oController, sMode, oClickXY, mOptions) {
        // ThemeService already normalizes unsupported modes to morning.
        // Preserve the API contract while making the current limitation explicit.
        var sResolvedMode = ThemeService.normalizeMode ? ThemeService.normalizeMode(sMode) : DEFAULT_MODE;
        if (sResolvedMode !== DEFAULT_MODE) {
            sResolvedMode = DEFAULT_MODE;
        }
        return applyMorningMode(oController, oClickXY, mOptions);
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
        setThemeMode: function (sMode, oClickXY) {
            return applySupportedMode(this, sMode, oClickXY);
        },
        applyStoredTheme: function () {
            return applyMorningMode(this, null);
        },
        toggleTheme: function (oClickXY) {
            return applySupportedMode(this, DEFAULT_MODE, oClickXY);
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
