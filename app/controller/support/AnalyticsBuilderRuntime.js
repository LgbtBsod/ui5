sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsPayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime"
], function (ControllerTextRuntime, AnalyticsPayloadNormalizer, ControllerViewStateRuntime) {
    "use strict";

    var getText = ControllerTextRuntime.getText;
    var BUILDER_DIMENSION_RULES = {
        MONTH: { vizType: "column", metricKeys: ["TOTAL", "FAILED_CHECKS", "FAILED_BARRIERS", "FAILED_CHECKLISTS", "FAILED_BARRIER_CHECKLISTS"], chartKeyByMetric: { TOTAL: "monthlyTotal", FAILED_CHECKS: "monthlyFailedChecks", FAILED_BARRIERS: "monthlyFailedBarriers", FAILED_CHECKLISTS: "monthlyFailedChecklists", FAILED_BARRIER_CHECKLISTS: "monthlyFailedBarrierChecklists" } },
        SOURCE: { vizType: "bar", metricKeys: ["TOTAL", "FAILED_CHECKS", "FAILED_BARRIERS"], chartKeyByMetric: { TOTAL: "totalBySource", FAILED_CHECKS: "failedChecksBySource", FAILED_BARRIERS: "failedBarriersBySource" } },
        PROFESSION: { vizType: "bar", metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"], chartKeyByMetric: { FAILED_CHECKS: "failedChecksByProfession", FAILED_BARRIERS: "failedBarriersByProfession" } },
        LPC: { vizType: "bar", metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"], chartKeyByMetric: { FAILED_CHECKS: "failedChecksByLpc", FAILED_BARRIERS: "failedBarriersByLpc" } },
        LOCATION: { vizType: "bar", metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"], chartKeyByMetric: { FAILED_CHECKS: "failedChecksByLocation", FAILED_BARRIERS: "failedBarriersByLocation" } },
        BUKRS: { vizType: "bar", metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"], chartKeyByMetric: { FAILED_CHECKS: "failedChecksByBukrs", FAILED_BARRIERS: "failedBarriersByBukrs" } },
        ORGUNIT: { vizType: "bar", metricKeys: ["FAILED_CHECKS", "FAILED_BARRIERS"], chartKeyByMetric: { FAILED_CHECKS: "failedChecksByOrgunit", FAILED_BARRIERS: "failedBarriersByOrgunit" } },
        BARRIER_NUMBER: { vizType: "bar", metricKeys: ["TOTAL", "FAILED_BARRIERS"], chartKeyByMetric: { TOTAL: "totalBarriersByBarrierNumber", FAILED_BARRIERS: "failedBarriersByBarrierNumber" } }
    };
    var BUILDER_DIMENSION_TEXT_KEY_MAP = { MONTH: "analyticsDimensionMonth", SOURCE: "analyticsDimensionSource", PROFESSION: "analyticsDimensionProfession", LPC: "analyticsDimensionLpc", LOCATION: "analyticsDimensionLocation", BUKRS: "analyticsDimensionBukrs", ORGUNIT: "analyticsDimensionOrgunit", BARRIER_NUMBER: "analyticsDimensionBarrierNumber" };
    var BUILDER_METRIC_TEXT_KEY_MAP = { TOTAL: "analyticsMetricTotal", FAILED_CHECKS: "analyticsMetricFailedChecks", FAILED_BARRIERS: "analyticsMetricFailedBarriers", FAILED_CHECKLISTS: "analyticsMetricFailedChecklistCount", FAILED_BARRIER_CHECKLISTS: "analyticsMetricFailedBarrierChecklistCount" };

    function normalizeBuilderDimension(sDimension) {
        var sResolved = String(sDimension || "").trim().toUpperCase();
        return BUILDER_DIMENSION_RULES[sResolved] ? sResolved : "MONTH";
    }

    function normalizeBuilderDimensionForSource(sDimension, sSource) {
        var sResolvedDimension = normalizeBuilderDimension(sDimension);
        var sResolvedSource = String(sSource || "ALL").trim().toUpperCase();
        if (sResolvedSource === "INTEGRATION" && ["LOCATION", "BUKRS", "ORGUNIT"].indexOf(sResolvedDimension) >= 0) {
            return "MONTH";
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
        var sResolvedSource = String(sSource || "ALL").trim().toUpperCase();
        var aDimensionKeys = ["MONTH", "SOURCE", "PROFESSION", "LPC", "BARRIER_NUMBER"];
        if (sResolvedSource !== "INTEGRATION") {
            aDimensionKeys = aDimensionKeys.concat(["LOCATION", "BUKRS", "ORGUNIT"]);
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
            busy: false, error: "", refreshBusy: false, selectedYear: String(iCurrentYear), compareYear: String(iCurrentYear - 1),
            compareYearValueState: "None", compareYearValueStateText: "", comparisonMetric: "FAILED_CHECKLISTS",
            builderDimension: "MONTH", builderMetric: "FAILED_CHECKLISTS", builderDimensionOptions: [], builderMetricOptions: [],
            builderChartRows: [], builderChartTitle: "", builderVizType: "column", builderChartHasData: false,
            builderSourceHintText: "", compareYearHasData: true, compareYearHintText: "",
            availableYears: [{ key: String(iCurrentYear), text: String(iCurrentYear) }], selectedSource: "ALL",
            refreshState: { taskKey: sTaskKey || "ANALYTICS_REFRESH", taskName: "Analytics Refresh", status: "IDLE", isRunning: false, requestedAt: "", requestedBy: "", startedAt: "", finishedAt: "", lastSuccessAt: "", lastError: "", lastMessage: "", activeRunId: "" },
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
        var sMetric = String(ControllerViewStateRuntime.get(oController, "/comparisonMetric", "FAILED_CHECKLISTS") || "FAILED_CHECKLISTS").trim().toUpperCase();
        var oAnalytics = ControllerViewStateRuntime.get(oController, "/analytics", {}) || {};
        var aRows = Array.isArray((oAnalytics.comparisonMetricSeries || {})[sMetric]) ? (oAnalytics.comparisonMetricSeries || {})[sMetric] : [];
        ControllerViewStateRuntime.set(oController, "/analytics/comparisonChartRows", aRows);
        ControllerViewStateRuntime.set(oController, "/comparisonMetric", sMetric);
    }

    function applyBuilderSelection(oController, mOverrides) {
        var oAnalytics = ControllerViewStateRuntime.get(oController, "/analytics", {}) || {};
        var sSelectedSource = String(ControllerViewStateRuntime.get(oController, "/selectedSource", "ALL") || "ALL").trim().toUpperCase();
        var sDimension = normalizeBuilderDimensionForSource((mOverrides && mOverrides.dimension) || ControllerViewStateRuntime.get(oController, "/builderDimension", "MONTH"), sSelectedSource);
        var sMetric = normalizeBuilderMetric(sDimension, (mOverrides && mOverrides.metric) || ControllerViewStateRuntime.get(oController, "/builderMetric", "FAILED_CHECKLISTS"));
        var aRows = resolveBuilderChartRows(oAnalytics, sDimension, sMetric);
        var sMetricText = getText(oController, BUILDER_METRIC_TEXT_KEY_MAP[sMetric], null, sMetric);
        var sDimensionText = getText(oController, BUILDER_DIMENSION_TEXT_KEY_MAP[sDimension], null, sDimension);
        ControllerViewStateRuntime.setMany(oController, {
            "/builderDimension": sDimension,
            "/builderMetric": sMetric,
            "/builderDimensionOptions": buildBuilderDimensionOptions(oController, sSelectedSource),
            "/builderMetricOptions": buildBuilderMetricOptions(oController, sDimension),
            "/builderChartRows": aRows,
            "/builderChartTitle": getText(oController, "analyticsBuilderTitlePattern", [sMetricText, sDimensionText], sMetricText + " by " + sDimensionText),
            "/builderVizType": BUILDER_DIMENSION_RULES[sDimension].vizType,
            "/builderChartHasData": Array.isArray(aRows) && aRows.length > 0
        });
    }

    function syncAnalyticsContextHints(oController) {
        var oAnalytics = ControllerViewStateRuntime.get(oController, "/analytics", {}) || {};
        var sSelectedSource = String(ControllerViewStateRuntime.get(oController, "/selectedSource", "ALL") || "ALL").trim().toUpperCase();
        var sBuilderDimension = String(ControllerViewStateRuntime.get(oController, "/builderDimension", "MONTH") || "MONTH").trim().toUpperCase();
        var iSelectedYear = Number(oAnalytics.selectedYear || 0);
        var iCompareYear = Number(ControllerViewStateRuntime.get(oController, "/compareYear", 0) || 0);
        var bCompareYearHasData = iSelectedYear === iCompareYear || !!oAnalytics.compareYearHasData;
        var sCompareYearHintText = bCompareYearHasData ? "" : getText(oController, "analyticsCompareYearNoData", [String(iCompareYear || "")], "No aggregated data for compare year " + String(iCompareYear || ""));
        var sBuilderSourceHintText = "";
        if (sSelectedSource === "INTEGRATION") {
            sBuilderSourceHintText = getText(oController, "analyticsIntegrationDimensionsNote", [], "Integration data can be analysed by month, LPC, profession, source and barrier number until enrichment fills BUKRS, location and observer org unit.");
        } else if (sSelectedSource === "ALL" && ["LOCATION", "BUKRS", "ORGUNIT"].indexOf(sBuilderDimension) >= 0) {
            sBuilderSourceHintText = getText(oController, "analyticsWebEnrichedDimensionsNote", [], "Web-enriched dimensions exclude incomplete integration records until enrichment fills BUKRS, location and observer org unit.");
        }
        ControllerViewStateRuntime.setMany(oController, {
            "/compareYearHasData": bCompareYearHasData,
            "/compareYearHintText": sCompareYearHintText,
            "/builderSourceHintText": sBuilderSourceHintText
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
