sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants"
], function (ModelContracts, StatePaths, MessageKeyConstants) {
    "use strict";

    var SEARCH_MESSAGE_KEYS = MessageKeyConstants.SEARCH;

    return Object.freeze({
        PATHS: Object.freeze({
            ANALYTICS_DRILLDOWN_INTENT: "/analyticsDrilldownIntent",
            SEARCH_SORT_KEY: "/searchSortKey",
            SEARCH_SORT_DESCENDING: "/searchSortDescending",
            SEARCH_GROUP_KEY: "/searchGroupKey",
            SEARCH_GROUP_DESCENDING: "/searchGroupDescending",
            SEARCH_MAX_RESULTS: "/searchMaxResults",
            SEARCH_BACKEND_TOP: "/searchBackendTop",
            SEARCH_FETCH_LIMIT: "/searchFetchLimit",
            GROWING_PAGE_SIZE: "/growingPageSize",
            SEARCH_MODE: StatePaths.SEARCH_MODE
        }),
        DEFAULTS: Object.freeze({
            SORT_KEY: ModelContracts.TOKENS.DATE_CHECK,
            GROUP_KEY: ModelContracts.TOKENS.GROUP_NONE
        }),
        SORT_ITEMS: Object.freeze([
            Object.freeze({ key: "DateCheck", textKey: SEARCH_MESSAGE_KEYS.SORT_DATE_CHECK }),
            Object.freeze({ key: "Id", textKey: SEARCH_MESSAGE_KEYS.SORT_ID }),
            Object.freeze({ key: "Status", textKey: SEARCH_MESSAGE_KEYS.SORT_STATUS }),
            Object.freeze({ key: "LpcText", textKey: SEARCH_MESSAGE_KEYS.SORT_LPC }),
            Object.freeze({ key: "ProfessionText", textKey: SEARCH_MESSAGE_KEYS.SORT_PROFESSION }),
            Object.freeze({ key: "ChangedOn", textKey: SEARCH_MESSAGE_KEYS.SORT_CHANGED_ON })
        ]),
        GROUP_ITEMS: Object.freeze([
            Object.freeze({ key: ModelContracts.TOKENS.GROUP_NONE, textKey: SEARCH_MESSAGE_KEYS.GROUP_NONE }),
            Object.freeze({ key: "Status", textKey: SEARCH_MESSAGE_KEYS.GROUP_STATUS }),
            Object.freeze({ key: "LpcText", textKey: SEARCH_MESSAGE_KEYS.GROUP_LPC }),
            Object.freeze({ key: "ProfessionText", textKey: SEARCH_MESSAGE_KEYS.GROUP_PROFESSION }),
            Object.freeze({ key: "DateCheck", textKey: SEARCH_MESSAGE_KEYS.GROUP_DATE_CHECK })
        ])
    });
});
