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
            closedCount: toNumber(o.closedCount || o.ClosedCount),
            registeredCount: toNumber(o.registeredCount || o.RegisteredCount),
            avgChecksRate: toNumber(o.avgChecksRate || o.AvgChecksRate),
            avgBarriersRate: toNumber(o.avgBarriersRate || o.AvgBarriersRate),
            refreshedAt: String(o.refreshedAt || o.RefreshedAt || "-"),
            source: String(o.source || o.Source || "backend")
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
            STATUS_TOTAL: "totalByStatus",
            MONTHLY_TOTAL_SELECTED: "monthlyTotalSelected",
            MONTHLY_TOTAL_PREVIOUS: "monthlyTotalPrevious",
            MONTHLY_FAILED_CHECKS_SELECTED: "monthlyFailedChecksSelected",
            MONTHLY_FAILED_CHECKS_PREVIOUS: "monthlyFailedChecksPrevious",
            MONTHLY_FAILED_BARRIERS_SELECTED: "monthlyFailedBarriersSelected",
            MONTHLY_FAILED_BARRIERS_PREVIOUS: "monthlyFailedBarriersPrevious"
        };
        var grouped = {
            failedChecksByProfession: [],
            failedBarriersByProfession: [],
            failedChecksByLpc: [],
            failedBarriersByLpc: [],
            failedChecksByLocation: [],
            failedBarriersByLocation: [],
            totalByStatus: [],
            monthlyTotalSelected: [],
            monthlyTotalPrevious: [],
            monthlyFailedChecksSelected: [],
            monthlyFailedChecksPrevious: [],
            monthlyFailedBarriersSelected: [],
            monthlyFailedBarriersPrevious: []
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
            totalByStatus: normalizeChartRows(grouped.totalByStatus),
            monthlyTotalSelected: normalizeChartRows(grouped.monthlyTotalSelected),
            monthlyTotalPrevious: normalizeChartRows(grouped.monthlyTotalPrevious),
            monthlyFailedChecksSelected: normalizeChartRows(grouped.monthlyFailedChecksSelected),
            monthlyFailedChecksPrevious: normalizeChartRows(grouped.monthlyFailedChecksPrevious),
            monthlyFailedBarriersSelected: normalizeChartRows(grouped.monthlyFailedBarriersSelected),
            monthlyFailedBarriersPrevious: normalizeChartRows(grouped.monthlyFailedBarriersPrevious)
        };
    }

    function create() {
        function readParams(mInput) {
            var iSelectedYear = toNumber(mInput && mInput.selectedYear);
            return iSelectedYear > 0 ? { year: iSelectedYear } : {};
        }

        return {
            fetchSummary: function (mInput) {
                return GatewayAdapterSupport.get("SimpleAnalyticalSet", readParams(mInput)).then(normalizeSummary);
            },
            fetchDetailed: function (mInput) {
                var mParams = readParams(mInput);
                return Promise.all([
                    GatewayAdapterSupport.get("SimpleAnalyticalSet", mParams),
                    GatewayAdapterSupport.get("WorkflowAnalyticsBreakdownSet", mParams)
                ]).then(function (aResult) {
                    return Object.assign({}, normalizeSummary(aResult[0]), {
                        charts: buildCharts(aResult[1])
                    });
                });
            }
        };
    }

    return { create: create };
});
