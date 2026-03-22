sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        EVENTS: Object.freeze({
            BEFORE_UNLOAD: "beforeunload",
            PAGE_HIDE: "pagehide"
        }),
        HASH_CHANGER: Object.freeze({
            GET_HASH: "getHash",
            REPLACE_HASH: "replaceHash"
        }),
        METHODS: Object.freeze({
            ADD_STYLE_CLASS: "addStyleClass",
            ABORT: "abort",
            ATTACH_AFTER_CLOSE: "attachAfterClose",
            ATTACH_AFTER_OPEN: "attachAfterOpen",
            ATTACH_THEME_CHANGED: "attachThemeChanged",
            CLOSE: "close",
            DESTROY: "destroy",
            DETACH_THEME_CHANGED: "detachThemeChanged",
            FOCUS: "focus",
            FUNCTION: "function",
            GET_CONFIGURATION: "getConfiguration",
            GET_CONTEXTS: "getContexts",
            GET_DOM_REF: "getDomRef",
            GET_FOCUS_DOM_REF: "getFocusDomRef",
            GET_LANGUAGE_TAG: "getLanguageTag",
            GET_MODEL: "getModel",
            GET_PROPERTY: "getProperty",
            GET_RESOURCE_BUNDLE: "getResourceBundle",
            GET_STATIC_AREA_REF: "getStaticAreaRef",
            GET_URL: "getURL",
            GET_VALUE: "getValue",
            NAV_TO: "navTo",
            OPEN: "open",
            OPEN_BY: "openBy",
            REMOVE_STYLE_CLASS: "removeStyleClass",
            REQUEST_CONTEXTS: "requestContexts",
            REPLACE_HASH: "replaceHash",
            SET_PROPERTY: "setProperty",
            SET_SELECTED_KEY: "setSelectedKey",
            SET_TOKENS: "setTokens",
            SET_VALUE: "setValue",
            SET_VALUES: "setValues",
            TO_STRING: "toString"
        }),
        TYPEOF: Object.freeze({
            BOOLEAN: "boolean",
            FUNCTION: "function",
            OBJECT: "object",
            UNDEFINED: "undefined"
        }),
        VALUES: Object.freeze({
            UNDEFINED: "undefined",
            WINDOW: "window"
        })
    });
});
