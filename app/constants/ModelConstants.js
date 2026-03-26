sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        MODELS: Object.freeze({
            DETAIL: "detail",
            DEVICE: "device",
            I18N: "i18n",
            MASTER_DATA: "masterData",
            SHELL: "shell",
            STATE: "state",
            VIEW: "view"
        }),
        MODEL_PATHS: Object.freeze({
            SHELL_ACTIVITY_IDLE_UNTIL: "/activity/idleUntil",
            SHELL_ACTIVITY_LAST_ACTIVE_AT: "/activity/lastActiveAt",
            SHELL_ANIMATION_ENABLED: "/animationEnabled",
            SHELL_BUSY: "/busy",
            SHELL_COMPACT_DENSITY: "/compactDensity",
            SHELL_CURRENT_DB_KEY: "/currentChecklistDbKey",
            SHELL_INVERTED_BLOCK_SCHEME: "/invertedBlockScheme",
            SHELL_IS_PHONE_VIEWPORT: "/isPhoneViewport",
            SHELL_IS_TABLET_VIEWPORT: "/isTabletViewport",
            SHELL_LAYOUT: "/layout",
            SHELL_LOCK: "/lock",
            SHELL_PERSONALIZATION_INFO_CARD_LAYOUT: "/personalization/infoCardLayout",
            SHELL_PERSONALIZATION_SHOW_HINTS: "/personalization/showHints",
            SHELL_ROOT: "/shell",
            SHELL_SESSION_GUID: "/sessionGuid",
            SHELL_SMART_FILTER_FIELDS: "/smartFilter/fields",
            SHELL_SMART_TABLE_COLUMNS: "/smartTable/columns",
            SHELL_SMART_TABLE_SELECTION_MODE: "/smartTable/selectionMode",
            SHELL_THEME_MODE: "/themeMode",
            SHELL_USER_ACTION_KIND: "/shell/userActionKind",
            SHELL_USER_REFRESH_BUSY: "/shell/userRefreshBusy",
            SHELL_VIEWPORT_WIDTH: "/viewportWidth"
        }),
        TOKENS: Object.freeze({
            ANALYTICS: "analytics",
            DATE_CHECK: "DateCheck",
            DETAIL: "detail",
            GROUP_NONE: "__NONE__"
        })
    });
});
