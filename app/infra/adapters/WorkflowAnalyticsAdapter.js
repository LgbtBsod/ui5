sap.ui.define([
    "sap_ui5/infra/adapters/shared/GatewayAdapterSupport"
], function (GatewayAdapterSupport) {
    "use strict";

    function toNumber(vValue) {
        var nValue = Number(vValue);
        return isFinite(nValue) ? nValue : 0;
    }

    function normalizeSummary(oData) {
        var oUnwrapped = GatewayAdapterSupport.unwrap(oData);
        var o = Array.isArray(oUnwrapped) ? (oUnwrapped[0] || {}) : (oUnwrapped || {});
        return {
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
            LPC_FAILED_BARRIERS: "failedBarriersByLpc"
        };
        var grouped = {
            failedChecksByProfession: [],
            failedBarriersByProfession: [],
            failedChecksByLpc: [],
            failedBarriersByLpc: []
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
            failedBarriersByLpc: normalizeChartRows(grouped.failedBarriersByLpc)
        };
    }

    function create() {
        return {
            fetchSummary: function () {
                return GatewayAdapterSupport.get("SimpleAnalyticalSet").then(normalizeSummary);
            },
            fetchDetailed: function () {
                return Promise.all([
                    GatewayAdapterSupport.get("SimpleAnalyticalSet"),
                    GatewayAdapterSupport.get("WorkflowAnalyticsBreakdownSet")
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
