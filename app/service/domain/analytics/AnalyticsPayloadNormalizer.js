sap.ui.define([
    "checklist/app/service/framework/ComponentRuntimeSupport"
], function (ComponentRuntimeSupport) {
    "use strict";

    function toNumber(vValue) {
        var nValue = Number(vValue);
        return isFinite(nValue) ? nValue : 0;
    }

    function formatAnalyticsDateTime(vDate) {
        if (vDate === null || vDate === undefined || vDate === "" || vDate === "-") {
            return "-";
        }
        return ComponentRuntimeSupport.formatHumanDateTime(vDate);
    }

    function normalizeChartRows(aRows) {
        var aSource = Array.isArray(aRows) ? aRows : [];
        var nMax = aSource.reduce(function (nAcc, oRow) {
            return Math.max(nAcc, toNumber(oRow && oRow.value));
        }, 0);

        return aSource.map(function (oRow, index) {
            var nValue = toNumber(oRow && oRow.value);
            var sLabel = String((oRow && oRow.label) || "");
            var nHeightRem = nMax > 0 ? Math.max(0.5, (nValue / nMax) * 7.5) : 0.5;

            return {
                label: sLabel,
                labelShort: sLabel.length > 12 ? sLabel.slice(0, 12) + "..." : sLabel,
                value: nValue,
                order: toNumber(oRow && oRow.order) || (index + 1),
                barHeight: nHeightRem.toFixed(2) + "rem"
            };
        });
    }

    function buildMonthlyComparison(oCharts) {
        var aSelectedTotal = normalizeChartRows(oCharts && oCharts.monthlyTotalSelected);
        var aPreviousTotal = normalizeChartRows(oCharts && oCharts.monthlyTotalPrevious);
        var aSelectedChecks = normalizeChartRows(oCharts && oCharts.monthlyFailedChecksSelected);
        var aPreviousChecks = normalizeChartRows(oCharts && oCharts.monthlyFailedChecksPrevious);
        var aSelectedBarriers = normalizeChartRows(oCharts && oCharts.monthlyFailedBarriersSelected);
        var aPreviousBarriers = normalizeChartRows(oCharts && oCharts.monthlyFailedBarriersPrevious);
        return aSelectedTotal.map(function (oRow, index) {
            var oPreviousTotal = aPreviousTotal[index] || {};
            var oSelectedChecksRow = aSelectedChecks[index] || {};
            var oPreviousChecksRow = aPreviousChecks[index] || {};
            var oSelectedBarriersRow = aSelectedBarriers[index] || {};
            var oPreviousBarriersRow = aPreviousBarriers[index] || {};
            return {
                monthLabel: oRow.label || oRow.labelShort || "",
                selectedTotal: toNumber(oRow.value),
                previousTotal: toNumber(oPreviousTotal.value),
                selectedFailedChecks: toNumber(oSelectedChecksRow.value),
                previousFailedChecks: toNumber(oPreviousChecksRow.value),
                selectedFailedBarriers: toNumber(oSelectedBarriersRow.value),
                previousFailedBarriers: toNumber(oPreviousBarriersRow.value)
            };
        });
    }

    function normalizeDashboard(oSummary) {
        var o = oSummary || {};
        var aFailedChecksByProfession = normalizeChartRows(o.charts && o.charts.failedChecksByProfession);
        var aFailedBarriersByProfession = normalizeChartRows(o.charts && o.charts.failedBarriersByProfession);
        var aFailedChecksByLpc = normalizeChartRows(o.charts && o.charts.failedChecksByLpc);
        var aFailedBarriersByLpc = normalizeChartRows(o.charts && o.charts.failedBarriersByLpc);
        var aFailedChecksByLocation = normalizeChartRows(o.charts && o.charts.failedChecksByLocation);
        var aFailedBarriersByLocation = normalizeChartRows(o.charts && o.charts.failedBarriersByLocation);
        var aTotalByStatus = normalizeChartRows(o.charts && o.charts.totalByStatus);
        var aMonthlyComparison = buildMonthlyComparison(o.charts || {});
        var aAvailableYears = (Array.isArray(o.availableYears) ? o.availableYears : []).map(function (oYear) {
            return {
                key: String((oYear && oYear.key) || ""),
                text: String((oYear && oYear.text) || (oYear && oYear.key) || "")
            };
        }).filter(function (oYear) {
            return !!oYear.key;
        });

        return {
            selectedYear: toNumber(o.selectedYear || o.SelectedYear),
            previousYear: toNumber(o.previousYear || o.PreviousYear),
            availableYears: aAvailableYears,
            total: toNumber(o.total || o.Total),
            failedChecks: toNumber(o.failedChecks || o.FailedChecks),
            failedBarriers: toNumber(o.failedBarriers || o.FailedBarriers),
            closedCount: toNumber(o.closedCount || o.ClosedCount),
            registeredCount: toNumber(o.registeredCount || o.RegisteredCount),
            avgChecksRate: toNumber(o.avgChecksRate || o.AvgChecksRate),
            avgBarriersRate: toNumber(o.avgBarriersRate || o.AvgBarriersRate),
            refreshedAt: formatAnalyticsDateTime(o.refreshedAt || o.RefreshedAt || ""),
            source: String(o.source || o.Source || "backend"),
            monthlyComparison: aMonthlyComparison,
            charts: {
                failedChecksByProfession: aFailedChecksByProfession,
                failedBarriersByProfession: aFailedBarriersByProfession,
                failedChecksByLpc: aFailedChecksByLpc,
                failedBarriersByLpc: aFailedBarriersByLpc,
                failedChecksByLocation: aFailedChecksByLocation,
                failedBarriersByLocation: aFailedBarriersByLocation,
                totalByStatus: aTotalByStatus
            },
            hasCharts: {
                failedChecksByProfession: aFailedChecksByProfession.length > 0,
                failedBarriersByProfession: aFailedBarriersByProfession.length > 0,
                failedChecksByLpc: aFailedChecksByLpc.length > 0,
                failedBarriersByLpc: aFailedBarriersByLpc.length > 0,
                failedChecksByLocation: aFailedChecksByLocation.length > 0,
                failedBarriersByLocation: aFailedBarriersByLocation.length > 0,
                totalByStatus: aTotalByStatus.length > 0,
                monthlyComparison: aMonthlyComparison.length > 0
            }
        };
    }

    function buildRailPayload(oSummary) {
        var o = oSummary || {};

        return {
            selectedYear: toNumber(o.selectedYear || o.SelectedYear),
            previousYear: toNumber(o.previousYear || o.PreviousYear),
            total: toNumber(o.total || o.Total),
            monthly: toNumber(o.monthly || o.Monthly),
            failedChecks: toNumber(o.failedChecks || o.FailedChecks),
            failedBarriers: toNumber(o.failedBarriers || o.FailedBarriers),
            avgChecksRate: toNumber(o.avgChecksRate || o.AvgChecksRate),
            avgBarriersRate: toNumber(o.avgBarriersRate || o.AvgBarriersRate),
            refreshedAtText: formatAnalyticsDateTime(o.refreshedAt || o.RefreshedAt || ""),
            sourceText: String(o.source || o.Source || "backend")
        };
    }

    function createEmptyDashboard() {
        return normalizeDashboard({});
    }

    return {
        buildRailPayload: buildRailPayload,
        createEmptyDashboard: createEmptyDashboard,
        formatAnalyticsDateTime: formatAnalyticsDateTime,
        normalizeDashboard: normalizeDashboard
    };
});
