sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService"], function (ThemeService) {
    "use strict";

    var DEFAULT_MODE = "morning";
    var DEFAULT_ANIMATION_ENABLED = ThemeService.DEFAULT_ANIMATION_ENABLED;

    function readThemeProfile() {
        var oProfile = ThemeService.getThemeProfile ? ThemeService.getThemeProfile() : null;
        return {
            mode: DEFAULT_MODE,
            animationEnabled: oProfile && typeof oProfile.animationEnabled === "boolean" ? oProfile.animationEnabled : DEFAULT_ANIMATION_ENABLED
        };
    }

    return {
        getCurrentTheme: function () {
            var oProfile = readThemeProfile();
            return ThemeService.themeForMode(oProfile.mode);
        },
        getCurrentThemeMode: function () {
            return DEFAULT_MODE;
        },
        isDarkThemeEnabled: function () { return false; },
        isThemeAnimationEnabled: function () {
            return !!readThemeProfile().animationEnabled;
        },
        setThemeAnimationEnabled: function (bEnabled) {
            var oProfile = ThemeService.setThemeAnimationEnabled
                ? ThemeService.setThemeAnimationEnabled(!!bEnabled)
                : { mode: this.getCurrentThemeMode(), animationEnabled: !!bEnabled };
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(oProfile.mode, null, {
                animationEnabled: oProfile.animationEnabled,
                persist: false
            });
        },
        setThemeMode: function (sMode, oClickXY) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(DEFAULT_MODE, oClickXY || null, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        applyStoredTheme: function () {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(oProfile.mode, null, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        toggleTheme: function (oClickXY) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(DEFAULT_MODE, oClickXY, {
                animationEnabled: oProfile.animationEnabled
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
                animationEnabled: oProfile.animationEnabled
            });
        }
    };
});
