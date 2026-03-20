sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (UiSemanticConstants) {
    "use strict";

    return Object.freeze({
        PATHS: Object.freeze({
            ANALYTICS: "/analytics",
            ANALYTICS_DRILLDOWN_INTENT: "/analyticsDrilldownIntent",
            AVAILABLE_YEARS: "/availableYears",
            BUILDER_CHART_HAS_DATA: "/builderChartHasData",
            BUILDER_CHART_ROWS: "/builderChartRows",
            BUILDER_CHART_TITLE: "/builderChartTitle",
            BUILDER_DIMENSION: "/builderDimension",
            BUILDER_DIMENSION_OPTIONS: "/builderDimensionOptions",
            BUILDER_METRIC: "/builderMetric",
            BUILDER_METRIC_OPTIONS: "/builderMetricOptions",
            BUILDER_SOURCE_HINT_TEXT: "/builderSourceHintText",
            BUILDER_VIZ_TYPE: "/builderVizType",
            BUSY: "/busy",
            COMPARISON_METRIC: "/comparisonMetric",
            COMPARE_YEAR: "/compareYear",
            COMPARE_YEAR_HAS_DATA: "/compareYearHasData",
            COMPARE_YEAR_HINT_TEXT: "/compareYearHintText",
            COMPARE_YEAR_OPTIONS: "/compareYearOptions",
            COMPARE_YEAR_VALUE_STATE: "/compareYearValueState",
            COMPARE_YEAR_VALUE_STATE_TEXT: "/compareYearValueStateText",
            ERROR: "/error",
            REFRESH_BUSY: "/refreshBusy",
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
            ERROR: UiSemanticConstants.VALUE_STATE.ERROR,
            NONE: UiSemanticConstants.VALUE_STATE.NONE
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
            ROUTE_MATCHED: "routeMatched",
            SOURCE_CHANGED: "sourceChanged",
            YEAR_CHANGED: "yearChanged",
            YEAR_PICKED: "yearPicked",
            YEAR_PRESET_CHANGED: "yearPresetChanged"
        }),
        MESSAGES: Object.freeze({
            ANALYTICS_LOAD_FAILED: "Analytics load failed",
            ANALYTICS_REFRESH_FAILED: "Analytics refresh failed",
            ANALYTICS_UNAVAILABLE: "Analytics unavailable",
            INVALID_YEAR: "Analytics year is invalid"
        })
    });
});
