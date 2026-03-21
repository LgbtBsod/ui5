sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        DETAIL_PATHS: Object.freeze({
            LOCATION_KEY: "/current/basic/LOCATION_KEY",
            LOCATION_NAME: "/current/basic/LOCATION_NAME",
            LOCATION_TEXT: "/current/basic/LOCATION_TEXT"
        }),
        DIALOG_ACTIONS: Object.freeze({
            CLOSE: "close",
            OPEN: "open"
        }),
        SESSION_CACHE_KEY: "pcct_detail_location_vh_cache_v1",
        DEFAULT_CACHE_KEY: "__default__",
        SEARCH_LIMIT: 200,
        INTENTS: Object.freeze({
            CONFIRM: "confirm",
            OPEN: "open",
            SEARCH: "search",
            TREE_SELECTION: "treeSelection"
        }),
        HINTS: Object.freeze({
            NO_DATA: "locationValueHelpNoDataHint"
        }),
        DIALOG_ID: "locationValueHelp",
        VIEW_PATHS: Object.freeze({
            CACHE_KEY: "/locationVhCacheKey",
            HAS_SELECTION: "/locationVhHasSelection",
            HINT: "/locationVhHint",
            LOADED: "/locationVhLoaded",
            SELECTION: "/locationVhSelection",
            TREE: "/locationVhTree",
            TREE_SOURCE: "/locationVhTreeSource"
        })
    });
});
