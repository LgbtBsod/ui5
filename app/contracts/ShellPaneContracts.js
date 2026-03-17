sap.ui.define([], function () {
    "use strict";

    var PANES = Object.freeze({
        SEARCH: "search",
        DETAIL: "detail",
        ANALYTICS: "analytics"
    });

    var PAGE_IDS = Object.freeze({
        search: "searchTargetPage",
        detail: "detailTargetPage",
        analytics: "analyticsTargetPage"
    });

    var READINESS_PATHS = Object.freeze({
        search: "/readiness/panes/search/loaded",
        detail: "/readiness/panes/detail/loaded",
        analytics: "/readiness/panes/analytics/loaded"
    });

    return Object.freeze({
        PANES: PANES,
        PAGE_IDS: PAGE_IDS,
        READINESS_PATHS: READINESS_PATHS
    });
});
