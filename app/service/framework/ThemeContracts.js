sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        CLASSES: Object.freeze({
            SWITCHING: "appThemeSwitching",
            MOTION_DISABLED: "theme-motion-disabled",
            MOTION_ENABLED: "theme-motion-enabled"
        }),
        STORAGE_KEYS: Object.freeze({
            PROFILE: "checklist_app_theme_profile",
            LEGACY_PROFILE: "sap_ui5_theme_profile",
            LEGACY_THEME: "sap_ui5_theme",
            PROFILE_RECOVERY: "checklist_app_theme_profile_recovery_20260305",
            APP_THEME: "checklist_app_theme"
        }),
        MODES: Object.freeze({
            DEFAULT: "morning",
            MORNING: "morning",
            NIGHT: "night"
        }),
        THEMES: Object.freeze({
            MORNING: "sap_fiori_3",
            NIGHT: "sap_fiori_3_dark"
        }),
        DURATIONS: Object.freeze({
            SWITCH_MS: 220,
            SWITCH_THEME_CHANGED_MIN_MS: 520
        }),
        DEFAULTS: Object.freeze({
            ANIMATION_ENABLED: true
        })
    });
});
