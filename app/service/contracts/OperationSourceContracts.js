sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        ANALYTICS: Object.freeze({
            DRILLDOWN: "analyticsDrilldown",
            EXPORT: "analyticsExport",
            REFRESH: "analyticsRefresh",
            YEAR_PRESET_CHANGED: "yearPresetChanged"
        }),
        APP: Object.freeze({
            ACTIVITY_MONITOR: "activityMonitor",
            CROSS_TAB: "crossTab",
            DETAIL_MATCHED: "detailMatched"
        }),
        DETAIL: Object.freeze({
            CLOSE: "close",
            CONFIRM: "confirm",
            MANUAL_CHANGE: "manualChange",
            OPEN: "open",
            SEARCH: "search",
            SELECTED: "selected",
            TREE_SELECTION: "treeSelection"
        }),
        SEARCH: Object.freeze({
            ANALYTICS_DRILLDOWN: "analyticsDrilldown",
            BACKEND_AGGREGATE: "backend_aggregate",
            BACKEND_TOP_CHANGE: "backendTopChange",
            BEFORE_REBIND: "beforeRebind",
            SEARCH_GROUP_SETTINGS: "searchGroupSettings",
            SEARCH_RETRY: "searchRetry",
            SEARCH_SORT_SETTINGS: "searchSortSettings",
            SMART_FILTER_CHANGED: "smartFilterChanged",
            SMART_FILTER_INIT: "smartFilterInit",
            SMART_SEARCH: "smartSearch",
            TABLE_ITEM_PRESS: "tableItemPress",
            TABLE_SELECTION: "tableSelection"
        })
    });
});
