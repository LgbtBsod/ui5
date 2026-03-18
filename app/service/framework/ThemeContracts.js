sap.ui.define([], function () {
    "use strict";

    var MODES = Object.freeze({
        DEFAULT: "morning",
        MORNING: "morning"
    });

    return Object.freeze({
        CLASSES: Object.freeze({
            MOTION_DISABLED: "theme-motion-disabled",
            MOTION_ENABLED: "theme-motion-enabled",
            SWITCHING: "appThemeSwitching"
        }),
        STORAGE_KEYS: Object.freeze({
            APP_THEME: "checklist_app_theme",
            DEV_OVERRIDE: "checklist_app_theme_dev_override",
            LEGACY_PROFILE: "sap_ui5_theme_profile",
            LEGACY_THEME: "sap_ui5_theme",
            PROFILE: "checklist_app_theme_profile",
            PROFILE_RECOVERY: "checklist_app_theme_profile_recovery_20260305"
        }),
        MODES: MODES,
        THEMES: Object.freeze({
            MORNING: "sap_fiori_3"
        }),
        DURATIONS: Object.freeze({
            SWITCH_MS: 220,
            SWITCH_THEME_CHANGED_MIN_MS: 520
        }),
        DEFAULTS: Object.freeze({
            ANIMATION_ENABLED: true
        }),
        APP_VIEW_PATHS: Object.freeze({
            ANIMATION_ENABLED: "/animationEnabled",
            INVERTED_BLOCK_SCHEME: "/invertedBlockScheme",
            THEME_MODE: "/themeMode"
        }),
        SYNC_SOURCES: Object.freeze({
            ANIMATION: "animation",
            INIT: "init",
            SETTINGS: "settings-theme-mode",
            TOGGLE: "toggle-locked-morning"
        }),
        DEV_OVERRIDE_MODES: Object.freeze({
            CLEAR: Object.freeze(["clear", "off"]),
            MORNING: Object.freeze(["dark", "light", "morning", "night"])
        })
    });
});
