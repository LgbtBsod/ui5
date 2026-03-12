sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/ThemeService"
], function (ThemeService) {
    "use strict";

    return {
        DEFAULT_MODE: ThemeService.DEFAULT_MODE,
        DEFAULT_ANIMATION_ENABLED: ThemeService.DEFAULT_ANIMATION_ENABLED,
        modeForTheme: function (sTheme) {
            return ThemeService.modeForTheme(sTheme);
        },
        themeForMode: function (sMode) {
            return ThemeService.themeForMode(sMode);
        },
        getThemeProfile: function () {
            return ThemeService.getThemeProfile();
        },
        setThemeProfile: function (oProfile) {
            return ThemeService.setThemeProfile(oProfile);
        },
        setThemeMode: function (sMode) {
            return ThemeService.setThemeMode(sMode);
        },
        setThemeAnimationEnabled: function (bEnabled) {
            return ThemeService.setThemeAnimationEnabled(bEnabled);
        },
        applyThemeMode: function (sMode, oClickXY, mOptions) {
            return ThemeService.applyThemeMode(sMode, oClickXY, mOptions);
        },
        syncTokensFromUI5: function () {
            return ThemeService.syncTokensFromUI5();
        },
        syncDocumentRootClasses: function () {
            return ThemeService.syncDocumentRootClasses();
        },
        syncAnimationClass: function (bEnabled) {
            return ThemeService.syncAnimationClass(bEnabled);
        },
        applyTheme: function (sTheme, oClickXY, mOptions) {
            return ThemeService.applyTheme(sTheme, oClickXY, mOptions);
        }
    };
});
