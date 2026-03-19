sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (ModelContracts) {
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
            SEARCH_MODE: "/searchMode"
        }),
        DEFAULTS: Object.freeze({
            SORT_KEY: ModelContracts.TOKENS.DATE_CHECK,
            GROUP_KEY: ModelContracts.TOKENS.GROUP_NONE
        }),
        SORT_ITEMS: Object.freeze([
            Object.freeze({ key: "DateCheck", textKey: "searchSortDateCheck", fallback: "Date" }),
            Object.freeze({ key: "Id", textKey: "searchSortId", fallback: "ID" }),
            Object.freeze({ key: "Status", textKey: "searchSortStatus", fallback: "Status" }),
            Object.freeze({ key: "LpcText", textKey: "searchSortLpc", fallback: "LPC" }),
            Object.freeze({ key: "ProfessionText", textKey: "searchSortProfession", fallback: "Profession" }),
            Object.freeze({ key: "ChangedOn", textKey: "searchSortChangedOn", fallback: "Changed on" })
        ]),
        GROUP_ITEMS: Object.freeze([
            Object.freeze({ key: "__NONE__", textKey: "searchGroupNone", fallback: "No grouping" }),
            Object.freeze({ key: "Status", textKey: "searchGroupStatus", fallback: "Status" }),
            Object.freeze({ key: "LpcText", textKey: "searchGroupLpc", fallback: "LPC" }),
            Object.freeze({ key: "ProfessionText", textKey: "searchGroupProfession", fallback: "Profession" }),
            Object.freeze({ key: "DateCheck", textKey: "searchGroupDateCheck", fallback: "Date" })
        ])
    });
});
