sap.ui.define([], function () {
    "use strict";

    return Object.freeze({
        PATHS: Object.freeze({
            ANALYTICS: "/analytics",
            AVAILABLE_YEARS: "/availableYears",
            BUSY: "/busy",
            COMPARE_YEAR: "/compareYear",
            COMPARE_YEAR_OPTIONS: "/compareYearOptions",
            ERROR: "/error",
            REFRESH_STATE: "/refreshState",
            SELECTED_SOURCE: "/selectedSource",
            SELECTED_YEAR: "/selectedYear",
            YEAR_PICKER_ITEMS: "/yearPicker/items",
            YEAR_PICKER_RANGE_END: "/yearPicker/rangeEnd",
            YEAR_PICKER_RANGE_LABEL: "/yearPicker/rangeLabel",
            YEAR_PICKER_RANGE_START: "/yearPicker/rangeStart",
            YEAR_PICKER_TARGET_FIELD: "/yearPicker/targetField"
        }),
        YEAR_PICKER_FIELDS: Object.freeze({
            COMPARE: "compareYear",
            SELECTED: "selectedYear"
        }),
        VALIDATION_STATES: Object.freeze({
            ERROR: "Error",
            NONE: "None"
        }),
        TEXT_KEYS: Object.freeze({
            COMPARE_YEAR_INVALID: "analyticsCompareYearInvalid"
        }),
        FRAGMENT_IDS: Object.freeze({
            YEAR_PICKER: "analyticsYearPicker"
        }),
        LOAD_REASONS: Object.freeze({
            COMPARE_YEAR_CHANGED: "compareYearChanged",
            COMPARE_YEAR_PICKED: "compareYearPicked",
            MANUAL: "manual",
            MANUAL_REFRESH: "manualRefresh",
            ROUTE_MATCHED: "routeMatched",
            SOURCE_CHANGED: "sourceChanged",
            YEAR_CHANGED: "yearChanged",
            YEAR_PICKED: "yearPicked",
            YEAR_PRESET_CHANGED: "yearPresetChanged"
        }),
        MESSAGES: Object.freeze({
            ANALYTICS_LOAD_FAILED: "Analytics load failed",
            INVALID_YEAR: "Analytics year is invalid"
        })
    });
});
