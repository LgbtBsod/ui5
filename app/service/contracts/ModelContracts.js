sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        MODELS: Object.freeze({
            APP_VIEW: "appView",
            I18N: "i18n",
            MASTER_DATA: "masterData",
            SELECTED: "selected",
            STATE: "state",
            VIEW: "view"
        }),
        MODEL_PATHS: Object.freeze({
            APP_VIEW_ANIMATION_ENABLED: "/animationEnabled",
            APP_VIEW_COMPACT_DENSITY: "/compactDensity",
            APP_VIEW_INVERTED_BLOCK_SCHEME: "/invertedBlockScheme",
            APP_VIEW_IS_PHONE_VIEWPORT: "/isPhoneViewport",
            APP_VIEW_IS_TABLET_VIEWPORT: "/isTabletViewport",
            APP_VIEW_SHELL: "/shell",
            APP_VIEW_SHELL_USER_ACTION_KIND: "/shell/userActionKind",
            APP_VIEW_VIEWPORT_WIDTH: "/viewportWidth"
        }),
        TOKENS: Object.freeze({
            ANALYTICS: "analytics",
            DATE_CHECK: "DateCheck",
            DETAIL: "detail",
            GROUP_NONE: "__NONE__"
        })
    });
});
