sap.ui.define(["checklist/app/service/framework/ThemeRuntime"], function (ThemeService) {
    "use strict";

    var DEFAULT_MODE = ThemeService.DEFAULT_MODE;
    var DEFAULT_ANIMATION_ENABLED = ThemeService.DEFAULT_ANIMATION_ENABLED;
    var DEFAULT_BACKGROUND_INTERACTIVE = ThemeService.DEFAULT_BACKGROUND_INTERACTIVE;

    function readThemeProfile() {
        var oProfile = ThemeService.getThemeProfile ? ThemeService.getThemeProfile() : null;
        return {
            mode: (oProfile && oProfile.mode) || DEFAULT_MODE,
            animationEnabled: oProfile && typeof oProfile.animationEnabled === "boolean" ? oProfile.animationEnabled : DEFAULT_ANIMATION_ENABLED,
            backgroundInteractive: oProfile && typeof oProfile.backgroundInteractive === "boolean" ? oProfile.backgroundInteractive : DEFAULT_BACKGROUND_INTERACTIVE
        };
    }

    return {
        getCurrentTheme: function () {
            var oProfile = readThemeProfile();
            return ThemeService.themeForMode(oProfile.mode);
        },
        getCurrentThemeMode: function () {
            return readThemeProfile().mode;
        },
        isDarkThemeEnabled: function () { return this.getCurrentThemeMode() === "night"; },
        isThemeAnimationEnabled: function () {
            return !!readThemeProfile().animationEnabled;
        },
        isBackgroundInteractiveEnabled: function () {
            return !!readThemeProfile().backgroundInteractive;
        },
        setThemeAnimationEnabled: function (bEnabled) {
            var oProfile = ThemeService.setThemeAnimationEnabled
                ? ThemeService.setThemeAnimationEnabled(!!bEnabled)
                : { mode: this.getCurrentThemeMode(), animationEnabled: !!bEnabled };
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(oProfile.mode, null, {
                animationEnabled: oProfile.animationEnabled,
                backgroundInteractive: oProfile.backgroundInteractive,
                persist: false
            });
        },
        setThemeBackgroundInteractive: function (bEnabled) {
            var oProfile = ThemeService.setThemeBackgroundInteractive
                ? ThemeService.setThemeBackgroundInteractive(!!bEnabled)
                : { mode: this.getCurrentThemeMode(), animationEnabled: this.isThemeAnimationEnabled(), backgroundInteractive: !!bEnabled };
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(oProfile.mode, null, {
                animationEnabled: oProfile.animationEnabled,
                backgroundInteractive: oProfile.backgroundInteractive,
                persist: false
            });
        },
        setThemeMode: function (sMode, oClickXY) {
            var oProfile = readThemeProfile();
            var sTarget = String(sMode || DEFAULT_MODE).toLowerCase() === "night" ? "night" : "morning";
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(sTarget, oClickXY || null, {
                animationEnabled: oProfile.animationEnabled,
                backgroundInteractive: oProfile.backgroundInteractive
            });
        },
        applyStoredTheme: function () {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(oProfile.mode, null, {
                animationEnabled: oProfile.animationEnabled,
                backgroundInteractive: oProfile.backgroundInteractive
            });
        },
        toggleTheme: function (oClickXY) {
            var oProfile = readThemeProfile();
            var sNextMode = this.isDarkThemeEnabled() ? "morning" : "night";
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(sNextMode, oClickXY, {
                animationEnabled: oProfile.animationEnabled,
                backgroundInteractive: oProfile.backgroundInteractive
            });
        },
        _ensureThemeSyncListener: function () {
            if (this._fnThemeChangedHandler) { return; }
            this._fnThemeChangedHandler = function () {
                ThemeService.syncTokensFromUI5();
            }.bind(this);
            sap.ui.getCore().attachThemeChanged(this._fnThemeChangedHandler);
        },
        _applyTheme: function (sTheme) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyTheme(sTheme, null, {
                animationEnabled: oProfile.animationEnabled,
                backgroundInteractive: oProfile.backgroundInteractive
            });
        }
    };
});
