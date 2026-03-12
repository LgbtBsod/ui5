sap.ui.define([], function () {
    "use strict";

    var PANES = Object.freeze({
        SEARCH: "search",
        DETAIL: "detail",
        ANALYTICS: "analytics"
    });

    var HOST_IDS = Object.freeze({
        SEARCH: "searchPaneHost",
        DETAIL: "detailPaneHost",
        ANALYTICS: "analyticsPaneHost"
    });

    var VIEW_IDS = Object.freeze({
        DETAIL: "detailPaneView",
        ANALYTICS: "analyticsPaneView"
    });

    var VIEW_NAMES = Object.freeze({
        SEARCH: "PRODUCTION_CONTROL_CHECKLIST.views.Search",
        DETAIL: "PRODUCTION_CONTROL_CHECKLIST.views.Detail",
        ANALYTICS: "PRODUCTION_CONTROL_CHECKLIST.views.Analytics"
    });

    var STYLE_IDS = Object.freeze({
        DETAIL: "chk-detail-pane-styles",
        ANALYTICS: "chk-analytics-pane-styles"
    });

    var STYLE_MODULES = Object.freeze({
        DETAIL: "PRODUCTION_CONTROL_CHECKLIST/styles/modules/41_page_detail.css",
        ANALYTICS: "PRODUCTION_CONTROL_CHECKLIST/styles/modules/42_page_analytics.css"
    });

    var READINESS_PATHS = Object.freeze({
        SEARCH: "/readiness/panes/search/loaded",
        DETAIL: "/readiness/panes/detail/loaded",
        ANALYTICS: "/readiness/panes/analytics/loaded"
    });

    var LAZY_PANES = Object.freeze([
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
        LAZY_PANES: LAZY_PANES
    });
});
