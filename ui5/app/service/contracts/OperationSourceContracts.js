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
            DIALOG_CLOSED: "dialogClosed",
            ENTER_EDIT: "enter_edit",
            ENTER_EDIT_TOGGLE: "enter_edit_toggle",
            MANUAL_CHANGE: "manualChange",
            OPEN: "open",
            SEARCH: "search",
            SELECTED: "selected",
            TEST_USER: "testUser",
            TREE_SELECTION: "treeSelection",
            VALIDATE: "validate"
        }),
        SEARCH: Object.freeze({
            ANALYTICS_DRILLDOWN: "analyticsDrilldown",
            BACKEND_AGGREGATE: "backend_aggregate",
            BACKEND_TOP_CHANGE: "backendTopChange",
            BARRIERS_SEGMENT: "barriersSegment",
            BEFORE_REBIND: "beforeRebind",
            CHECKS_SEGMENT: "checksSegment",
            COPY: "copy",
            OPEN: "open",
            SEARCH_GROUP_SETTINGS: "searchGroupSettings",
            SEARCH_MODE_TOGGLE: "searchModeToggle",
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
