sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (UiSemanticConstants) {
    "use strict";

    var SOURCES = Object.freeze({
        ALL: "ALL",
        WEB: "WEB",
        INTEGRATION: "INTEGRATION"
    });

    var YEAR_PRESETS = Object.freeze({
        CURRENT: "CURRENT",
        PREVIOUS: "PREVIOUS"
    });

    var REFRESH = Object.freeze({
        TASK_KEY: "ANALYTICS_REFRESH",
        TASK_NAME: "Analytics Refresh",
        REQUESTED_BY_WEB: SOURCES.WEB,
        POLL_DELAY_MS: 1800,
        POLL_MAX_ATTEMPTS: 12,
        STATUSES: Object.freeze({
            IDLE: "IDLE",
            REQUESTED: "REQUESTED",
            RUNNING: "RUNNING"
        })
    });

    var DIMENSIONS = Object.freeze({
        MONTH: "MONTH",
        SOURCE: "SOURCE",
        PROFESSION: "PROFESSION",
        LPC: "LPC",
        LOCATION: "LOCATION",
        BUKRS: "BUKRS",
        ORGUNIT: "ORGUNIT",
        BARRIER_NUMBER: "BARRIER_NUMBER"
    });

    var METRICS = Object.freeze({
        TOTAL: "TOTAL",
        FAILED_CHECKS: "FAILED_CHECKS",
        FAILED_BARRIERS: "FAILED_BARRIERS",
        FAILED_CHECKLISTS: "FAILED_CHECKLISTS",
        FAILED_BARRIER_CHECKLISTS: "FAILED_BARRIER_CHECKLISTS"
    });

    var BUILDER = Object.freeze({
        FALLBACK_DIMENSION: DIMENSIONS.MONTH,
        FALLBACK_COMPARISON_METRIC: METRICS.FAILED_CHECKLISTS,
        FALLBACK_SOURCE: SOURCES.ALL,
        DIMENSION_OPTIONS_BASE: Object.freeze([
            DIMENSIONS.MONTH,
            DIMENSIONS.SOURCE,
            DIMENSIONS.PROFESSION,
            DIMENSIONS.LPC,
            DIMENSIONS.BARRIER_NUMBER
        ]),
        SOURCE_RESTRICTED_DIMENSIONS: Object.freeze([
            DIMENSIONS.LOCATION,
            DIMENSIONS.BUKRS,
            DIMENSIONS.ORGUNIT
        ]),
        DIMENSION_RULES: Object.freeze({
            MONTH: Object.freeze({ vizType: "column", metricKeys: Object.freeze([METRICS.TOTAL, METRICS.FAILED_CHECKS, METRICS.FAILED_BARRIERS, METRICS.FAILED_CHECKLISTS, METRICS.FAILED_BARRIER_CHECKLISTS]), chartKeyByMetric: Object.freeze({ TOTAL: "monthlyTotal", FAILED_CHECKS: "monthlyFailedChecks", FAILED_BARRIERS: "monthlyFailedBarriers", FAILED_CHECKLISTS: "monthlyFailedChecklists", FAILED_BARRIER_CHECKLISTS: "monthlyFailedBarrierChecklists" }) }),
            SOURCE: Object.freeze({ vizType: "bar", metricKeys: Object.freeze([METRICS.TOTAL, METRICS.FAILED_CHECKS, METRICS.FAILED_BARRIERS]), chartKeyByMetric: Object.freeze({ TOTAL: "totalBySource", FAILED_CHECKS: "failedChecksBySource", FAILED_BARRIERS: "failedBarriersBySource" }) }),
            PROFESSION: Object.freeze({ vizType: "bar", metricKeys: Object.freeze([METRICS.FAILED_CHECKS, METRICS.FAILED_BARRIERS]), chartKeyByMetric: Object.freeze({ FAILED_CHECKS: "failedChecksByProfession", FAILED_BARRIERS: "failedBarriersByProfession" }) }),
            LPC: Object.freeze({ vizType: "bar", metricKeys: Object.freeze([METRICS.FAILED_CHECKS, METRICS.FAILED_BARRIERS]), chartKeyByMetric: Object.freeze({ FAILED_CHECKS: "failedChecksByLpc", FAILED_BARRIERS: "failedBarriersByLpc" }) }),
            LOCATION: Object.freeze({ vizType: "bar", metricKeys: Object.freeze([METRICS.FAILED_CHECKS, METRICS.FAILED_BARRIERS]), chartKeyByMetric: Object.freeze({ FAILED_CHECKS: "failedChecksByLocation", FAILED_BARRIERS: "failedBarriersByLocation" }) }),
            BUKRS: Object.freeze({ vizType: "bar", metricKeys: Object.freeze([METRICS.FAILED_CHECKS, METRICS.FAILED_BARRIERS]), chartKeyByMetric: Object.freeze({ FAILED_CHECKS: "failedChecksByBukrs", FAILED_BARRIERS: "failedBarriersByBukrs" }) }),
            ORGUNIT: Object.freeze({ vizType: "bar", metricKeys: Object.freeze([METRICS.FAILED_CHECKS, METRICS.FAILED_BARRIERS]), chartKeyByMetric: Object.freeze({ FAILED_CHECKS: "failedChecksByOrgunit", FAILED_BARRIERS: "failedBarriersByOrgunit" }) }),
            BARRIER_NUMBER: Object.freeze({ vizType: "bar", metricKeys: Object.freeze([METRICS.TOTAL, METRICS.FAILED_BARRIERS]), chartKeyByMetric: Object.freeze({ TOTAL: "totalBarriersByBarrierNumber", FAILED_BARRIERS: "failedBarriersByBarrierNumber" }) })
        }),
        DIMENSION_TEXT_KEYS: Object.freeze({
            MONTH: "analyticsDimensionMonth",
            SOURCE: "analyticsDimensionSource",
            PROFESSION: "analyticsDimensionProfession",
            LPC: "analyticsDimensionLpc",
            LOCATION: "analyticsDimensionLocation",
            BUKRS: "analyticsDimensionBukrs",
            ORGUNIT: "analyticsDimensionOrgunit",
            BARRIER_NUMBER: "analyticsDimensionBarrierNumber"
        }),
        METRIC_TEXT_KEYS: Object.freeze({
            TOTAL: "analyticsMetricTotal",
            FAILED_CHECKS: "analyticsMetricFailedChecks",
            FAILED_BARRIERS: "analyticsMetricFailedBarriers",
            FAILED_CHECKLISTS: "analyticsMetricFailedChecklistCount",
            FAILED_BARRIER_CHECKLISTS: "analyticsMetricFailedBarrierChecklistCount"
        })
    });

    var LOAD_STATUS = Object.freeze({
        ERROR: "error",
        LOADING: "loading",
        READY: "ready"
    });

    var REFRESH_STATUS = Object.freeze({
        ERROR: "ERROR",
        IDLE: "IDLE",
        READY: "READY",
        REQUESTED: "REQUESTED",
        RUNNING: "RUNNING",
        SUCCESS: "SUCCESS"
    });

    var REFRESH_MESSAGE_TYPE = Object.freeze({
        ACTIVE: UiSemanticConstants.MESSAGE_TYPE.INFORMATION,
        ERROR: UiSemanticConstants.MESSAGE_TYPE.WARNING,
        INFO: UiSemanticConstants.MESSAGE_TYPE.INFORMATION
    });

    var PATHS = Object.freeze({
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
    });

    return Object.freeze({
        SOURCES: SOURCES,
        YEAR_PRESETS: YEAR_PRESETS,
        REFRESH: REFRESH,
        DIMENSIONS: DIMENSIONS,
        METRICS: METRICS,
        BUILDER: BUILDER,
        LOAD_STATUS: LOAD_STATUS,
        REFRESH_STATUS: REFRESH_STATUS,
        REFRESH_MESSAGE_TYPE: REFRESH_MESSAGE_TYPE,
        PATHS: PATHS,
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
            ANALYTICS_LOAD_FAILED: "analytics_load_failed",
            ANALYTICS_REFRESH_FAILED: "analytics_refresh_failed",
            ANALYTICS_UNAVAILABLE: "analytics_unavailable",
            INVALID_YEAR: "analytics_year_invalid"
        })
    });
});
