sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchMessageKeyConstants"
], function (ModelContracts, StatePaths, SearchMessageKeyConstants) {
    "use strict";

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
            Object.freeze({ key: "DateCheck", textKey: SearchMessageKeyConstants.SORT_DATE_CHECK }),
            Object.freeze({ key: "Id", textKey: SearchMessageKeyConstants.SORT_ID }),
            Object.freeze({ key: "Status", textKey: SearchMessageKeyConstants.SORT_STATUS }),
            Object.freeze({ key: "LpcText", textKey: SearchMessageKeyConstants.SORT_LPC }),
            Object.freeze({ key: "ProfessionText", textKey: SearchMessageKeyConstants.SORT_PROFESSION }),
            Object.freeze({ key: "ChangedOn", textKey: SearchMessageKeyConstants.SORT_CHANGED_ON })
        ]),
        GROUP_ITEMS: Object.freeze([
            Object.freeze({ key: ModelContracts.TOKENS.GROUP_NONE, textKey: SearchMessageKeyConstants.GROUP_NONE }),
            Object.freeze({ key: "Status", textKey: SearchMessageKeyConstants.GROUP_STATUS }),
            Object.freeze({ key: "LpcText", textKey: SearchMessageKeyConstants.GROUP_LPC }),
            Object.freeze({ key: "ProfessionText", textKey: SearchMessageKeyConstants.GROUP_PROFESSION }),
            Object.freeze({ key: "DateCheck", textKey: SearchMessageKeyConstants.GROUP_DATE_CHECK })
        ])
    });
});
