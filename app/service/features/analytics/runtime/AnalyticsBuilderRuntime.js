sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsPayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/analytics/runtime/AnalyticsViewStateReader"
], function (ControllerTextRuntime, AnalyticsPayloadNormalizer, ControllerViewStateRuntime, AnalyticsContracts, AnalyticsUiContracts, AnalyticsViewStateReader) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var BUILDER_DIMENSION_RULES = AnalyticsContracts.BUILDER.DIMENSION_RULES;
    var BUILDER_DIMENSION_TEXT_KEY_MAP = AnalyticsContracts.BUILDER.DIMENSION_TEXT_KEYS;
    var BUILDER_METRIC_TEXT_KEY_MAP = AnalyticsContracts.BUILDER.METRIC_TEXT_KEYS;
    var FALLBACK_DIMENSION = AnalyticsContracts.BUILDER.FALLBACK_DIMENSION;
    var FALLBACK_METRIC = AnalyticsContracts.BUILDER.FALLBACK_COMPARISON_METRIC;
    var ALL_SOURCE = AnalyticsContracts.SOURCES.ALL;
    var INTEGRATION_SOURCE = AnalyticsContracts.SOURCES.INTEGRATION;
    var PATHS = AnalyticsUiContracts.PATHS;

    function normalizeBuilderDimension(sDimension) {
        var sResolved = String(sDimension || "").trim().toUpperCase();
        return BUILDER_DIMENSION_RULES[sResolved] ? sResolved : FALLBACK_DIMENSION;
    }

    function normalizeBuilderDimensionForSource(sDimension, sSource) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        var sResolvedSource = String(sSource || ALL_SOURCE).trim().toUpperCase();
        if (sResolvedSource === INTEGRATION_SOURCE && AnalyticsContracts.BUILDER.SOURCE_RESTRICTED_DIMENSIONS.indexOf(sResolvedDimension) >= 0) {
            return FALLBACK_DIMENSION;
        }
        return sResolvedDimension;
    }

    function normalizeBuilderMetric(sDimension, sMetric) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        var oRule = BUILDER_DIMENSION_RULES[sResolvedDimension];
        var sResolvedMetric = String(sMetric || "").trim().toUpperCase();
        return oRule.metricKeys.indexOf(sResolvedMetric) >= 0 ? sResolvedMetric : oRule.metricKeys[0];
    }

    function buildBuilderDimensionOptions(oController, sSource) {
        var sResolvedSource = String(sSource || ALL_SOURCE).trim().toUpperCase();
        var aDimensionKeys = AnalyticsContracts.BUILDER.DIMENSION_OPTIONS_BASE.slice();
        if (sResolvedSource !== INTEGRATION_SOURCE) {
            aDimensionKeys = aDimensionKeys.concat(AnalyticsContracts.BUILDER.SOURCE_RESTRICTED_DIMENSIONS);
        }
        return aDimensionKeys.map(function (sDimensionKey) {
            return { key: sDimensionKey, text: getText(oController, BUILDER_DIMENSION_TEXT_KEY_MAP[sDimensionKey], null, sDimensionKey) };
        });
    }

    function buildBuilderMetricOptions(oController, sDimension) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        return (BUILDER_DIMENSION_RULES[sResolvedDimension].metricKeys || []).map(function (sMetricKey) {
            return { key: sMetricKey, text: getText(oController, BUILDER_METRIC_TEXT_KEY_MAP[sMetricKey], null, sMetricKey) };
        });
    }

    function resolveBuilderChartRows(oAnalytics, sDimension, sMetric) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        var sResolvedMetric = normalizeBuilderMetric(sResolvedDimension, sMetric);
        var oRule = BUILDER_DIMENSION_RULES[sResolvedDimension];
        var sChartKey = oRule && oRule.chartKeyByMetric && oRule.chartKeyByMetric[sResolvedMetric];
        return sChartKey && oAnalytics && oAnalytics.charts && Array.isArray(oAnalytics.charts[sChartKey]) ? oAnalytics.charts[sChartKey] : [];
    }

    function createInitialViewState(sTaskKey) {
        var iCurrentYear = new Date().getFullYear();
        return {
            busy: false,
            error: "",
            refreshBusy: false,
            selectedYear: String(iCurrentYear),
            compareYear: String(iCurrentYear - 1),
            compareYearValueState: "None",
            compareYearValueStateText: "",
            comparisonMetric: FALLBACK_METRIC,
            builderDimension: FALLBACK_DIMENSION,
            builderMetric: FALLBACK_METRIC,
            builderDimensionOptions: [],
            builderMetricOptions: [],
            builderChartRows: [],
            builderChartTitle: "",
            builderVizType: "column",
            builderChartHasData: false,
            builderSourceHintText: "",
            compareYearHasData: true,
            compareYearHintText: "",
            yearPicker: { targetField: "selectedYear", rangeStart: iCurrentYear - 9, rangeEnd: iCurrentYear + 10, rangeLabel: "", items: [] },
            availableYears: [{ key: String(iCurrentYear), text: String(iCurrentYear) }],
            selectedSource: ALL_SOURCE,
            refreshState: { taskKey: sTaskKey || AnalyticsContracts.REFRESH.TASK_KEY, taskName: AnalyticsContracts.REFRESH.TASK_NAME, status: AnalyticsContracts.REFRESH.STATUSES.IDLE, isRunning: false, requestedAt: "", requestedBy: "", startedAt: "", finishedAt: "", lastSuccessAt: "", lastError: "", lastMessage: "", activeRunId: "" },
            analytics: AnalyticsPayloadNormalizer.createEmptyDashboard()
        };
    }

    function getSelectedKeyFromEvent(oEvent) {
        return String(
            oEvent && oEvent.getParameter && oEvent.getParameter("selectedItem") && oEvent.getParameter("selectedItem").getKey() ||
            oEvent && oEvent.getParameter && oEvent.getParameter("selectedKey") ||
            oEvent && oEvent.getSource && oEvent.getSource().getSelectedKey && oEvent.getSource().getSelectedKey() ||
            ""
        ).trim().toUpperCase();
    }

    function applyComparisonMetricSelection(oController) {
        var sMetric = String(ControllerViewStateRuntime.get(oController, PATHS.COMPARISON_METRIC, FALLBACK_METRIC) || FALLBACK_METRIC).trim().toUpperCase();
        var oAnalytics = AnalyticsViewStateReader.readAnalyticsRoot(oController);
        var aRows = Array.isArray((oAnalytics.comparisonMetricSeries || {})[sMetric]) ? (oAnalytics.comparisonMetricSeries || {})[sMetric] : [];
        ControllerViewStateRuntime.set(oController, PATHS.ANALYTICS + "/comparisonChartRows", aRows);
        ControllerViewStateRuntime.set(oController, PATHS.COMPARISON_METRIC, sMetric);
    }

    function applyBuilderSelection(oController, mOverrides) {
        var oAnalytics = AnalyticsViewStateReader.readAnalyticsRoot(oController);
        var oSelectionState = AnalyticsViewStateReader.readSelectionState(oController);
        var sDimension = normalizeBuilderDimensionForSource((mOverrides && mOverrides.dimension) || ControllerViewStateRuntime.get(oController, PATHS.BUILDER_DIMENSION, FALLBACK_DIMENSION), oSelectionState.selectedSource);
        var sMetric = normalizeBuilderMetric(sDimension, (mOverrides && mOverrides.metric) || ControllerViewStateRuntime.get(oController, PATHS.BUILDER_METRIC, FALLBACK_METRIC));
        var aRows = resolveBuilderChartRows(oAnalytics, sDimension, sMetric);
        var sMetricText = getText(oController, BUILDER_METRIC_TEXT_KEY_MAP[sMetric], null, sMetric);
        var sDimensionText = getText(oController, BUILDER_DIMENSION_TEXT_KEY_MAP[sDimension], null, sDimension);
        ControllerViewStateRuntime.setMany(oController, {
            [PATHS.BUILDER_DIMENSION]: sDimension,
            [PATHS.BUILDER_METRIC]: sMetric,
            [PATHS.BUILDER_DIMENSION_OPTIONS]: buildBuilderDimensionOptions(oController, oSelectionState.selectedSource),
            [PATHS.BUILDER_METRIC_OPTIONS]: buildBuilderMetricOptions(oController, sDimension),
            [PATHS.BUILDER_CHART_ROWS]: aRows,
            [PATHS.BUILDER_CHART_TITLE]: getText(oController, "analyticsBuilderTitlePattern", [sMetricText, sDimensionText], sMetricText + " by " + sDimensionText),
            [PATHS.BUILDER_VIZ_TYPE]: BUILDER_DIMENSION_RULES[sDimension].vizType,
            [PATHS.BUILDER_CHART_HAS_DATA]: Array.isArray(aRows) && aRows.length > 0
        });
    }

    function syncAnalyticsContextHints(oController) {
        var oAnalytics = AnalyticsViewStateReader.readAnalyticsRoot(oController);
        var oSelectionState = AnalyticsViewStateReader.readSelectionState(oController);
        var sBuilderDimension = String(ControllerViewStateRuntime.get(oController, PATHS.BUILDER_DIMENSION, FALLBACK_DIMENSION) || FALLBACK_DIMENSION).trim().toUpperCase();
        var iSelectedYear = Number(oAnalytics.selectedYear || 0);
        var iCompareYear = Number(oSelectionState.compareYear || 0);
        var bCompareYearHasData = iSelectedYear === iCompareYear || !!oAnalytics.compareYearHasData;
        var sCompareYearHintText = bCompareYearHasData ? "" : getText(oController, "analyticsCompareYearNoData", [String(iCompareYear || "")], "No aggregated data for compare year " + String(iCompareYear || ""));
        var sBuilderSourceHintText = "";
        if (oSelectionState.selectedSource === INTEGRATION_SOURCE) {
            sBuilderSourceHintText = getText(oController, "analyticsIntegrationDimensionsNote", [], "Integration data can be analysed by month, LPC, profession, source and barrier number until enrichment fills BUKRS, location and observer org unit.");
        } else if (oSelectionState.selectedSource === ALL_SOURCE && AnalyticsContracts.BUILDER.SOURCE_RESTRICTED_DIMENSIONS.indexOf(sBuilderDimension) >= 0) {
            sBuilderSourceHintText = getText(oController, "analyticsWebEnrichedDimensionsNote", [], "Web-enriched dimensions exclude incomplete integration records until enrichment fills BUKRS, location and observer org unit.");
        }
        ControllerViewStateRuntime.setMany(oController, {
            [PATHS.COMPARE_YEAR_HAS_DATA]: bCompareYearHasData,
            [PATHS.COMPARE_YEAR_HINT_TEXT]: sCompareYearHintText,
            [PATHS.BUILDER_SOURCE_HINT_TEXT]: sBuilderSourceHintText
        });
    }

    return {
        createInitialViewState: createInitialViewState,
        getSelectedKeyFromEvent: getSelectedKeyFromEvent,
        applyComparisonMetricSelection: applyComparisonMetricSelection,
        applyBuilderSelection: applyBuilderSelection,
        syncAnalyticsContextHints: syncAnalyticsContextHints
    };
});
