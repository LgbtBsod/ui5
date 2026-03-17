sap.ui.define([], function () {
    "use strict";

    var PANES = Object.freeze({
        SEARCH: "search",
        DETAIL: "detail",
        ANALYTICS: "analytics"
    });

    var HOST_IDS = Object.freeze({
        search: "searchPaneHost",
        detail: "detailPaneHost",
        analytics: "analyticsPaneHost"
    });

    var VIEW_IDS = Object.freeze({
        search: "searchPaneView",
        detail: "detailPaneView",
        analytics: "analyticsPaneView"
    });

    var VIEW_NAMES = Object.freeze({
        search: "PRODUCTION_CONTROL_CHECKLIST.views.Search",
        detail: "PRODUCTION_CONTROL_CHECKLIST.views.Detail",
        analytics: "PRODUCTION_CONTROL_CHECKLIST.views.Analytics"
    });

    var STYLE_IDS = Object.freeze({
        detail: "chk-detail-pane-styles",
        analytics: "chk-analytics-pane-styles"
    });

    var STYLE_MODULES = Object.freeze({
        detail: "PRODUCTION_CONTROL_CHECKLIST/styles/modules/41_page_detail.css",
        analytics: "PRODUCTION_CONTROL_CHECKLIST/styles/modules/42_page_analytics.css"
    });

    var READINESS_PATHS = Object.freeze({
        search: "/readiness/panes/search/loaded",
        detail: "/readiness/panes/detail/loaded",
        analytics: "/readiness/panes/analytics/loaded"
    });

    var PREWARM_DELAYS_MS = Object.freeze({
        detail: 80,
        analytics: 140
    });

    var LAZY_PANES = Object.freeze([
        PANES.SEARCH,
        PANES.DETAIL,
        PANES.ANALYTICS
    ]);

    return Object.freeze({
        PANES: PANES,
        HOST_IDS: HOST_IDS,
        VIEW_IDS: VIEW_IDS,
        VIEW_NAMES: VIEW_NAMES,
        STYLE_IDS: STYLE_IDS,
        STYLE_MODULES: STYLE_MODULES,
        READINESS_PATHS: READINESS_PATHS,
        PREWARM_DELAYS_MS: PREWARM_DELAYS_MS,
        LAZY_PANES: LAZY_PANES
    });
});
