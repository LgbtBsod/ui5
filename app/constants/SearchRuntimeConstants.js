sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        DEFAULTS: Object.freeze({
            SEARCH_BACKEND_TOP: "100",
            SEARCH_VISIBLE_ROWS: "100"
        }),
        PERSISTENCY_PREFIXES: Object.freeze({
            SMART_FILTER_SESSION: "pcctSmartFilterSession_",
            SMART_TABLE_SESSION: "pcctSmartTableSession_"
        }),
        SEARCH_MODE: Object.freeze({
            EXACT: "EXACT",
            LOOSE: "LOOSE"
        })
    });
});
