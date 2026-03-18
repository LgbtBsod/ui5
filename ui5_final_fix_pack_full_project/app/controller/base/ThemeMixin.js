sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (ThemeService, Ui5RuntimeFacade) {
    "use strict";

    var DEFAULT_MODE = "morning";
    var DEFAULT_ANIMATION_ENABLED = ThemeService.DEFAULT_ANIMATION_ENABLED;
    var DEV_OVERRIDE_STORAGE_KEY = "checklist_app_theme_dev_override";

    function isDebugOverrideAllowed() {
        if (typeof window === "undefined") {
            return false;
        }
        return window.__DEBUG_UI5__ === true;
    }

    function readDevOverrideMode() {
        var sStored = "";
        var sQuery = "";
        if (!isDebugOverrideAllowed()) {
            return "";
        }
        if (typeof window === "undefined") {
            return "";
        }
        try {
            sStored = String(window.localStorage.getItem(DEV_OVERRIDE_STORAGE_KEY) || "").trim().toLowerCase();
        } catch (_storageError) {
            sStored = "";
        }
        try {
            sQuery = String(new URLSearchParams(window.location.search).get("themeOverride") || "").trim().toLowerCase();
        } catch (_queryError) {
            sQuery = "";
        }
        if (sQuery === "dark" || sQuery === "night") {
            try {
                window.localStorage.setItem(DEV_OVERRIDE_STORAGE_KEY, "night");
            } catch (_persistNightError) {
                // Best-effort persistence only.
            }
            return "night";
        }
        if (sQuery === "light" || sQuery === "morning") {
            try {
                window.localStorage.setItem(DEV_OVERRIDE_STORAGE_KEY, "morning");
            } catch (_persistMorningError) {
                // Best-effort persistence only.
            }
            return "morning";
        }
        if (sQuery === "clear" || sQuery === "off") {
            try {
                window.localStorage.removeItem(DEV_OVERRIDE_STORAGE_KEY);
            } catch (_clearError) {
                // Best-effort persistence only.
            }
            return "";
        }
        return sStored === "night" ? "night" : sStored === "morning" ? "morning" : "";
    }

    function readThemeProfile() {
        var oProfile = ThemeService.getThemeProfile ? ThemeService.getThemeProfile() : null;
        var sMode = readDevOverrideMode() || (oProfile && oProfile.mode) || DEFAULT_MODE;
        return {
            mode: sMode,
            animationEnabled: oProfile && typeof oProfile.animationEnabled === "boolean" ? oProfile.animationEnabled : DEFAULT_ANIMATION_ENABLED
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
            var sNextMode = String(sMode || "").trim().toLowerCase();
            if (sNextMode !== "night" && sNextMode !== "morning") {
                sNextMode = DEFAULT_MODE;
            }
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(sNextMode, oClickXY || null, {
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
            var sNextMode = oProfile.mode === "night" ? "morning" : "night";
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(sNextMode, oClickXY, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        _ensureThemeSyncListener: function () {
            if (this._fnThemeChangedHandler) { return; }
            this._fnThemeChangedHandler = function () {
                ThemeService.syncTokensFromUI5();
            }.bind(this);
            Ui5RuntimeFacade.attachThemeChanged(this._fnThemeChangedHandler);
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
