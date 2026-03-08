sap.ui.define([
    "checklist/app/infra/adapters/shared/GatewayAdapterSupport"
], function (GatewayAdapterSupport) {
    "use strict";

    function toNumber(vValue) {
        var nValue = Number(vValue);
        return isFinite(nValue) ? nValue : 0;
    }

    function normalizeSummary(oData) {
        var oUnwrapped = GatewayAdapterSupport.unwrap(oData);
        var o = Array.isArray(oUnwrapped) ? (oUnwrapped[0] || {}) : (oUnwrapped || {});
        var aAvailableYears = [];
        try {
            aAvailableYears = JSON.parse(String(o.availableYearsJson || o.AvailableYearsJson || "[]"));
        } catch (oError) {
            aAvailableYears = [];
        }
        return {
            selectedYear: toNumber(o.selectedYear || o.SelectedYear),
            previousYear: toNumber(o.previousYear || o.PreviousYear),
            availableYears: Array.isArray(aAvailableYears) ? aAvailableYears : [],
            total: toNumber(o.total || o.Total),
            monthly: toNumber(o.monthly || o.Monthly),
            failedChecks: toNumber(o.failedChecks || o.FailedChecks),
            failedBarriers: toNumber(o.failedBarriers || o.FailedBarriers),
            failedChecklistCount: toNumber(o.failedChecklistCount || o.FailedChecklistCount),
            failedBarrierChecklistCount: toNumber(o.failedBarrierChecklistCount || o.FailedBarrierChecklistCount),
            closedCount: toNumber(o.closedCount || o.ClosedCount),
            registeredCount: toNumber(o.registeredCount || o.RegisteredCount),
            avgChecksRate: toNumber(o.avgChecksRate || o.AvgChecksRate),
            avgBarriersRate: toNumber(o.avgBarriersRate || o.AvgBarriersRate),
            refreshedAt: String(o.refreshedAt || o.RefreshedAt || "-"),
            source: String(o.source || o.Source || "ALL"),
            sourceText: String(o.sourceText || o.SourceText || o.source || o.Source || "All")
        };
    }

    function normalizeChartRows(vRows) {
        return (Array.isArray(vRows) ? vRows : []).map(function (oRow, index) {
            return {
                label: String((oRow && (oRow.label || oRow.Label || oRow.bucket_text || oRow.bucketText)) || ""),
                value: toNumber(oRow && (oRow.value || oRow.Value || oRow.metric_value || oRow.metricValue)),
                order: toNumber(oRow && (oRow.order || oRow.Order || index + 1))
            };
        });
    }

    function buildCharts(vRows) {
        var aRows = GatewayAdapterSupport.asArray(vRows);
        var mTargetByPair = {
            PROFESSION_FAILED_CHECKS: "failedChecksByProfession",
            PROFESSION_FAILED_BARRIERS: "failedBarriersByProfession",
            LPC_FAILED_CHECKS: "failedChecksByLpc",
            LPC_FAILED_BARRIERS: "failedBarriersByLpc",
            LOCATION_FAILED_CHECKS: "failedChecksByLocation",
            LOCATION_FAILED_BARRIERS: "failedBarriersByLocation",
            BUKRS_FAILED_CHECKS: "failedChecksByBukrs",
            BUKRS_FAILED_BARRIERS: "failedBarriersByBukrs",
            SOURCE_TOTAL: "totalBySource",
            SOURCE_FAILED_CHECKS: "failedChecksBySource",
            SOURCE_FAILED_BARRIERS: "failedBarriersBySource",
            BARRIER_NUMBER_FAILED_BARRIERS: "failedBarriersByBarrierNumber",
            MONTHLY_TOTAL: "monthlyTotal",
            MONTHLY_FAILED_CHECKS: "monthlyFailedChecks",
            MONTHLY_FAILED_BARRIERS: "monthlyFailedBarriers",
            MONTHLY_FAILED_CHECKLISTS: "monthlyFailedChecklists",
            MONTHLY_FAILED_BARRIER_CHECKLISTS: "monthlyFailedBarrierChecklists"
        };
        var grouped = {
            failedChecksByProfession: [],
            failedBarriersByProfession: [],
            failedChecksByLpc: [],
            failedBarriersByLpc: [],
            failedChecksByLocation: [],
            failedBarriersByLocation: [],
            failedChecksByBukrs: [],
            failedBarriersByBukrs: [],
            totalBySource: [],
            failedChecksBySource: [],
            failedBarriersBySource: [],
            failedBarriersByBarrierNumber: [],
            monthlyTotal: [],
            monthlyFailedChecks: [],
            monthlyFailedBarriers: [],
            monthlyFailedChecklists: [],
            monthlyFailedBarrierChecklists: []
        };
        aRows.forEach(function (oRow) {
            var sDimension = String((oRow && (oRow.Dimension || oRow.dimension)) || "").toUpperCase();
            var sMetric = String((oRow && (oRow.Metric || oRow.metric)) || "").toUpperCase();
            var sTarget = mTargetByPair[sDimension + "_" + sMetric] || "";
            if (sTarget) {
                grouped[sTarget].push({
                    label: String((oRow && (oRow.Label || oRow.label)) || ""),
                    value: toNumber(oRow && (oRow.Value || oRow.value)),
                    order: toNumber(oRow && (oRow.Order || oRow.order))
                });
            }
        });
        return {
            failedChecksByProfession: normalizeChartRows(grouped.failedChecksByProfession),
            failedBarriersByProfession: normalizeChartRows(grouped.failedBarriersByProfession),
            failedChecksByLpc: normalizeChartRows(grouped.failedChecksByLpc),
            failedBarriersByLpc: normalizeChartRows(grouped.failedBarriersByLpc),
            failedChecksByLocation: normalizeChartRows(grouped.failedChecksByLocation),
            failedBarriersByLocation: normalizeChartRows(grouped.failedBarriersByLocation),
            failedChecksByBukrs: normalizeChartRows(grouped.failedChecksByBukrs),
            failedBarriersByBukrs: normalizeChartRows(grouped.failedBarriersByBukrs),
            totalBySource: normalizeChartRows(grouped.totalBySource),
            failedChecksBySource: normalizeChartRows(grouped.failedChecksBySource),
            failedBarriersBySource: normalizeChartRows(grouped.failedBarriersBySource),
            failedBarriersByBarrierNumber: normalizeChartRows(grouped.failedBarriersByBarrierNumber),
            monthlyTotal: normalizeChartRows(grouped.monthlyTotal),
            monthlyFailedChecks: normalizeChartRows(grouped.monthlyFailedChecks),
            monthlyFailedBarriers: normalizeChartRows(grouped.monthlyFailedBarriers),
            monthlyFailedChecklists: normalizeChartRows(grouped.monthlyFailedChecklists),
            monthlyFailedBarrierChecklists: normalizeChartRows(grouped.monthlyFailedBarrierChecklists)
        };
    }

    function resolveCompareYear(mInput, iSelectedYear) {
        var iCompareYear = toNumber(mInput && mInput.compareYear);
        if (iCompareYear > 0) {
            return iCompareYear;
        }
        return iSelectedYear > 0 ? (iSelectedYear - 1) : 0;
    }

    function create() {
        function readParams(mInput) {
            var iSelectedYear = toNumber(mInput && mInput.selectedYear);
            var sSelectedSource = String(mInput && mInput.selectedSource || "").trim();
            var mParams = iSelectedYear > 0 ? { year: iSelectedYear } : {};
            if (sSelectedSource) {
                mParams.source = sSelectedSource;
            }
            return mParams;
        }

        function normalizeRefreshState(oData) {
            var oUnwrapped = GatewayAdapterSupport.unwrap(oData);
            var o = Array.isArray(oUnwrapped) ? (oUnwrapped[0] || {}) : (oUnwrapped || {});
            return {
                taskKey: String(o.taskKey || o.TaskKey || "ANALYTICS_REFRESH"),
                taskName: String(o.taskName || o.TaskName || "Analytics Refresh"),
                status: String(o.status || o.Status || "IDLE"),
                isRunning: !!(o.isRunning || o.IsRunning),
                requestedAt: String(o.requestedAt || o.RequestedAt || ""),
                requestedBy: String(o.requestedBy || o.RequestedBy || ""),
                startedAt: String(o.startedAt || o.StartedAt || ""),
                finishedAt: String(o.finishedAt || o.FinishedAt || ""),
                lastSuccessAt: String(o.lastSuccessAt || o.LastSuccessAt || ""),
                lastError: String(o.lastError || o.LastError || ""),
                lastMessage: String(o.lastMessage || o.LastMessage || ""),
                activeRunId: String(o.activeRunId || o.ActiveRunId || "")
            };
        }

        return {
            fetchSummary: function (mInput) {
                return GatewayAdapterSupport.get("SimpleAnalyticalSet", readParams(mInput)).then(normalizeSummary);
            },
            fetchDetailed: function (mInput) {
                var mParams = readParams(mInput);
                var iSelectedYear = toNumber(mInput && mInput.selectedYear);
                var iCompareYear = resolveCompareYear(mInput, iSelectedYear);
                var mCompareParams = readParams({
                    selectedYear: iCompareYear,
                    selectedSource: mInput && mInput.selectedSource
                });
                var pCompareBreakdown = iCompareYear === iSelectedYear
                    ? GatewayAdapterSupport.get("WorkflowAnalyticsBreakdownSet", mParams)
                    : GatewayAdapterSupport.get("WorkflowAnalyticsBreakdownSet", mCompareParams);
                return Promise.all([
                    GatewayAdapterSupport.get("SimpleAnalyticalSet", mParams),
                    GatewayAdapterSupport.get("WorkflowAnalyticsBreakdownSet", mParams),
                    GatewayAdapterSupport.get("AnalyticsRefreshStateSet('ANALYTICS_REFRESH')"),
                    pCompareBreakdown
                ]).then(function (aResult) {
                    return Object.assign({}, normalizeSummary(aResult[0]), {
                        charts: buildCharts(aResult[1]),
                        refreshState: normalizeRefreshState(aResult[2]),
                        compareYear: iCompareYear,
                        comparisonCharts: buildCharts(aResult[3])
                    });
                });
            },
            fetchRefreshState: function () {
                return GatewayAdapterSupport.get("AnalyticsRefreshStateSet('ANALYTICS_REFRESH')").then(normalizeRefreshState);
            },
            requestRefresh: function (mInput) {
                return GatewayAdapterSupport.postFunction("AnalyticsRefreshTrigger", {
                    TaskKey: "ANALYTICS_REFRESH",
                    RequestedBy: String(mInput && mInput.requestedBy || "")
                }).then(function () {
                    return GatewayAdapterSupport.get("AnalyticsRefreshStateSet('ANALYTICS_REFRESH')");
                }).then(normalizeRefreshState);
            }
        };
    }

    return { create: create };
});
