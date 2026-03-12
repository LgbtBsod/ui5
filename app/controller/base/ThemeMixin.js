sap.ui.define(["PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeRuntime"], function (ThemeRuntime) {
    "use strict";

    var DEFAULT_MODE = "morning";
    var DEFAULT_ANIMATION_ENABLED = ThemeRuntime.DEFAULT_ANIMATION_ENABLED;

    function readThemeProfile() {
        var oProfile = ThemeRuntime.getThemeProfile ? ThemeRuntime.getThemeProfile() : null;
        return {
            mode: DEFAULT_MODE,
            animationEnabled: oProfile && typeof oProfile.animationEnabled === "boolean" ? oProfile.animationEnabled : DEFAULT_ANIMATION_ENABLED
        };
    }

    return {
        getCurrentTheme: function () {
            var oProfile = readThemeProfile();
            return ThemeRuntime.themeForMode(oProfile.mode);
        },
        getCurrentThemeMode: function () {
            return DEFAULT_MODE;
        },
        isDarkThemeEnabled: function () { return false; },
        isThemeAnimationEnabled: function () {
            return !!readThemeProfile().animationEnabled;
        },
        setThemeAnimationEnabled: function (bEnabled) {
            var oProfile = ThemeRuntime.setThemeAnimationEnabled
                ? ThemeRuntime.setThemeAnimationEnabled(!!bEnabled)
                : { mode: this.getCurrentThemeMode(), animationEnabled: !!bEnabled };
            this._ensureThemeSyncListener();
            return ThemeRuntime.applyThemeMode(oProfile.mode, null, {
                animationEnabled: oProfile.animationEnabled,
                persist: false
            });
        },
        setThemeMode: function (sMode, oClickXY) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeRuntime.applyThemeMode(DEFAULT_MODE, oClickXY || null, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        applyStoredTheme: function () {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeRuntime.applyThemeMode(oProfile.mode, null, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        toggleTheme: function (oClickXY) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeRuntime.applyThemeMode(DEFAULT_MODE, oClickXY, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        _ensureThemeSyncListener: function () {
            if (this._fnThemeChangedHandler) { return; }
            this._fnThemeChangedHandler = function () {
                ThemeRuntime.syncTokensFromUI5();
            }.bind(this);
            sap.ui.getCore().attachThemeChanged(this._fnThemeChangedHandler);
        },
        _applyTheme: function (sTheme) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeRuntime.applyTheme(sTheme, null, {
                animationEnabled: oProfile.animationEnabled
            });
        }
    };
});
