sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (ThemeContracts, ThemeService, Ui5RuntimeFacade) {
    "use strict";

    var DEFAULT_MODE = ThemeContracts.MODES.MORNING;
    var DEFAULT_ANIMATION_ENABLED = ThemeService.DEFAULT_ANIMATION_ENABLED;
    var DEV_OVERRIDE_STORAGE_KEY = ThemeContracts.STORAGE_KEYS.DEV_OVERRIDE;
    var MORNING_OVERRIDE_VALUES = ThemeContracts.DEV_OVERRIDE_MODES.MORNING;
    var CLEAR_OVERRIDE_VALUES = ThemeContracts.DEV_OVERRIDE_MODES.CLEAR;

    function isDebugOverrideAllowed() {
        if (typeof window === "undefined") {
            return false;
        }
        return window.__DEBUG_UI5__ === true;
    }

    function persistMorningOverride() {
        try {
            window.localStorage.setItem(DEV_OVERRIDE_STORAGE_KEY, ThemeContracts.MODES.MORNING);
        } catch (_persistMorningError) {
            // Best-effort persistence only.
        }
    }

    function clearOverrideStorage() {
        try {
            window.localStorage.removeItem(DEV_OVERRIDE_STORAGE_KEY);
        } catch (_clearError) {
            // Best-effort persistence only.
        }
    }

    function readQueryOverride() {
        try {
            return String(new URLSearchParams(window.location.search).get("themeOverride") || "").trim().toLowerCase();
        } catch (_queryError) {
            return "";
        }
    }

    function readDevOverrideMode() {
        var sStored = "";
        var sQuery = "";

        if (!isDebugOverrideAllowed() || typeof window === "undefined") {
            return "";
        }

        try {
            sStored = String(window.localStorage.getItem(DEV_OVERRIDE_STORAGE_KEY) || "").trim().toLowerCase();
        } catch (_storageError) {
            sStored = "";
        }

        sQuery = readQueryOverride();
        if (MORNING_OVERRIDE_VALUES.indexOf(sQuery) >= 0) {
            persistMorningOverride();
            return ThemeContracts.MODES.MORNING;
        }
        if (CLEAR_OVERRIDE_VALUES.indexOf(sQuery) >= 0) {
            clearOverrideStorage();
            return "";
        }
        return sStored === ThemeContracts.MODES.MORNING ? ThemeContracts.MODES.MORNING : "";
    }

    function readThemeProfile() {
        var oProfile = ThemeService.getThemeProfile ? ThemeService.getThemeProfile() : null;
        var sMode = readDevOverrideMode() || DEFAULT_MODE;
        return {
            mode: ThemeContracts.MODES.MORNING,
            animationEnabled: oProfile && typeof oProfile.animationEnabled === "boolean" ? oProfile.animationEnabled : DEFAULT_ANIMATION_ENABLED,
            requestedMode: sMode
        };
    }

    return {
        getCurrentTheme: function () {
            return ThemeService.themeForMode(ThemeContracts.MODES.MORNING);
        },
        getCurrentThemeMode: function () {
            return ThemeContracts.MODES.MORNING;
        },
        isDarkThemeEnabled: function () {
            return false;
        },
        isThemeAnimationEnabled: function () {
            return !!readThemeProfile().animationEnabled;
        },
        setThemeAnimationEnabled: function (bEnabled) {
            var oProfile = ThemeService.setThemeAnimationEnabled
                ? ThemeService.setThemeAnimationEnabled(!!bEnabled)
                : { mode: ThemeContracts.MODES.MORNING, animationEnabled: !!bEnabled };
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(ThemeContracts.MODES.MORNING, null, {
                animationEnabled: oProfile.animationEnabled,
                persist: false
            });
        },
        setThemeMode: function (_sMode, oClickXY) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(ThemeContracts.MODES.MORNING, oClickXY || null, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        applyStoredTheme: function () {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(ThemeContracts.MODES.MORNING, null, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        toggleTheme: function (oClickXY) {
            var oProfile = readThemeProfile();
            this._ensureThemeSyncListener();
            return ThemeService.applyThemeMode(ThemeContracts.MODES.MORNING, oClickXY, {
                animationEnabled: oProfile.animationEnabled
            });
        },
        _ensureThemeSyncListener: function () {
            if (this._fnThemeChangedHandler) {
                return;
            }
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
