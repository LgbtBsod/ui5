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

    function toMonthlyMap(aRows) {
        var mValues = {};
        normalizeChartRows(aRows).forEach(function (oRow, index) {
            var iMonth = toNumber(oRow && oRow.order) || (index + 1);
            mValues[iMonth] = toNumber(oRow && oRow.value);
        });
        return mValues;
    }

    function monthProperty(iMonth) {
        return ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"][Math.max(1, Math.min(12, iMonth)) - 1];
    }

    function buildMonthlyComparison(oSelectedCharts, oCompareCharts) {
        var mSelectedTotal = toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyTotal);
        var mCompareTotal = toMonthlyMap(oCompareCharts && oCompareCharts.monthlyTotal);
        var mSelectedChecks = toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedChecks);
        var mCompareChecks = toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedChecks);
        var mSelectedBarriers = toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedBarriers);
        var mCompareBarriers = toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedBarriers);
        var mSelectedFailedChecklists = toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedChecklists);
        var mCompareFailedChecklists = toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedChecklists);
        var mSelectedFailedBarrierChecklists = toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedBarrierChecklists);
        var mCompareFailedBarrierChecklists = toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedBarrierChecklists);
        return Array.apply(null, new Array(12)).map(function (_vUnused, index) {
            var iMonth = index + 1;
            return {
                monthIndex: iMonth,
                monthLabel: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"][index],
                selectedTotal: toNumber(mSelectedTotal[iMonth]),
                compareTotal: toNumber(mCompareTotal[iMonth]),
                selectedFailedChecks: toNumber(mSelectedChecks[iMonth]),
                compareFailedChecks: toNumber(mCompareChecks[iMonth]),
                selectedFailedBarriers: toNumber(mSelectedBarriers[iMonth]),
                compareFailedBarriers: toNumber(mCompareBarriers[iMonth]),
                selectedFailedChecklists: toNumber(mSelectedFailedChecklists[iMonth]),
                compareFailedChecklists: toNumber(mCompareFailedChecklists[iMonth]),
                selectedFailedBarrierChecklists: toNumber(mSelectedFailedBarrierChecklists[iMonth]),
                compareFailedBarrierChecklists: toNumber(mCompareFailedBarrierChecklists[iMonth])
            };
        });
    }

    function buildMetricSeriesRows(aRows, sMetricKey) {
        return (Array.isArray(aRows) ? aRows : []).map(function (oRow) {
            var mMetricMap = {
                TOTAL: ["selectedTotal", "compareTotal"],
                FAILED_CHECKS: ["selectedFailedChecks", "compareFailedChecks"],
                FAILED_BARRIERS: ["selectedFailedBarriers", "compareFailedBarriers"],
                FAILED_CHECKLISTS: ["selectedFailedChecklists", "compareFailedChecklists"],
                FAILED_BARRIER_CHECKLISTS: ["selectedFailedBarrierChecklists", "compareFailedBarrierChecklists"]
            };
            var aProps = mMetricMap[sMetricKey] || mMetricMap.FAILED_CHECKS;
            return {
                monthLabel: String(oRow && oRow.monthLabel || ""),
                selectedValue: toNumber(oRow && oRow[aProps[0]]),
                compareValue: toNumber(oRow && oRow[aProps[1]])
            };
        });
    }

    function buildMonthlyMatrixRows(aRows, iSelectedYear, iCompareYear) {
        var aMetricDefs = [
            { key: "TOTAL", selectedProp: "selectedTotal", compareProp: "compareTotal" },
            { key: "FAILED_CHECKS", selectedProp: "selectedFailedChecks", compareProp: "compareFailedChecks" },
            { key: "FAILED_BARRIERS", selectedProp: "selectedFailedBarriers", compareProp: "compareFailedBarriers" },
            { key: "FAILED_CHECKLISTS", selectedProp: "selectedFailedChecklists", compareProp: "compareFailedChecklists" },
            { key: "FAILED_BARRIER_CHECKLISTS", selectedProp: "selectedFailedBarrierChecklists", compareProp: "compareFailedBarrierChecklists" }
        ];
        return aMetricDefs.reduce(function (aResult, oMetric) {
            var oSelectedRow = {
                metricKey: oMetric.key,
                yearLabel: String(iSelectedYear || ""),
                order: aResult.length + 1
            };
            var oCompareRow = {
                metricKey: oMetric.key,
                yearLabel: String(iCompareYear || ""),
                order: aResult.length + 2
            };
            (Array.isArray(aRows) ? aRows : []).forEach(function (oRow) {
                var sMonthProp = monthProperty(toNumber(oRow && oRow.monthIndex));
                oSelectedRow[sMonthProp] = toNumber(oRow && oRow[oMetric.selectedProp]);
                oCompareRow[sMonthProp] = toNumber(oRow && oRow[oMetric.compareProp]);
            });
            aResult.push(oSelectedRow, oCompareRow);
            return aResult;
        }, []);
    }

    function normalizeRefreshState(oRefreshState) {
        var o = oRefreshState || {};
        return {
            taskKey: String(o.taskKey || "ANALYTICS_REFRESH"),
            taskName: String(o.taskName || "Analytics Refresh"),
            status: String(o.status || "IDLE"),
            isRunning: !!o.isRunning,
            requestedAt: formatAnalyticsDateTime(o.requestedAt || ""),
            requestedBy: String(o.requestedBy || ""),
            startedAt: formatAnalyticsDateTime(o.startedAt || ""),
            finishedAt: formatAnalyticsDateTime(o.finishedAt || ""),
            lastSuccessAt: formatAnalyticsDateTime(o.lastSuccessAt || ""),
            lastError: String(o.lastError || ""),
            lastMessage: String(o.lastMessage || ""),
            activeRunId: String(o.activeRunId || "")
        };
    }

    function normalizeDashboard(oSummary) {
        var o = oSummary || {};
        var aFailedChecksByProfession = normalizeChartRows(o.charts && o.charts.failedChecksByProfession);
        var aFailedBarriersByProfession = normalizeChartRows(o.charts && o.charts.failedBarriersByProfession);
        var aFailedChecksByLpc = normalizeChartRows(o.charts && o.charts.failedChecksByLpc);
        var aFailedBarriersByLpc = normalizeChartRows(o.charts && o.charts.failedBarriersByLpc);
        var aFailedChecksByLocation = normalizeChartRows(o.charts && o.charts.failedChecksByLocation);
        var aFailedBarriersByLocation = normalizeChartRows(o.charts && o.charts.failedBarriersByLocation);
        var aFailedChecksByBukrs = normalizeChartRows(o.charts && o.charts.failedChecksByBukrs);
        var aFailedBarriersByBukrs = normalizeChartRows(o.charts && o.charts.failedBarriersByBukrs);
        var aTotalBySource = normalizeChartRows(o.charts && o.charts.totalBySource);
        var aFailedChecksBySource = normalizeChartRows(o.charts && o.charts.failedChecksBySource);
        var aFailedBarriersBySource = normalizeChartRows(o.charts && o.charts.failedBarriersBySource);
        var aFailedBarriersByBarrierNumber = normalizeChartRows(o.charts && o.charts.failedBarriersByBarrierNumber);
        var iSelectedYear = toNumber(o.selectedYear || o.SelectedYear);
        var iCompareYear = toNumber(o.compareYear || o.CompareYear || o.previousYear || o.PreviousYear);
        var aMonthlyComparison = buildMonthlyComparison(o.charts || {}, o.comparisonCharts || {});
        var mComparisonMetricSeries = {
            TOTAL: buildMetricSeriesRows(aMonthlyComparison, "TOTAL"),
            FAILED_CHECKS: buildMetricSeriesRows(aMonthlyComparison, "FAILED_CHECKS"),
            FAILED_BARRIERS: buildMetricSeriesRows(aMonthlyComparison, "FAILED_BARRIERS"),
            FAILED_CHECKLISTS: buildMetricSeriesRows(aMonthlyComparison, "FAILED_CHECKLISTS"),
            FAILED_BARRIER_CHECKLISTS: buildMetricSeriesRows(aMonthlyComparison, "FAILED_BARRIER_CHECKLISTS")
        };
        var aMonthlyMatrixRows = buildMonthlyMatrixRows(aMonthlyComparison, iSelectedYear, iCompareYear);
        var aAvailableYears = (Array.isArray(o.availableYears) ? o.availableYears : []).map(function (oYear) {
            return {
                key: String((oYear && oYear.key) || ""),
                text: String((oYear && oYear.text) || (oYear && oYear.key) || "")
            };
        }).filter(function (oYear) {
            return !!oYear.key;
        });

        return {
            selectedYear: iSelectedYear,
            previousYear: toNumber(o.previousYear || o.PreviousYear),
            compareYear: iCompareYear,
            availableYears: aAvailableYears,
            total: toNumber(o.total || o.Total),
            failedChecks: toNumber(o.failedChecks || o.FailedChecks),
            failedBarriers: toNumber(o.failedBarriers || o.FailedBarriers),
            failedChecklistCount: toNumber(o.failedChecklistCount || o.FailedChecklistCount),
            failedBarrierChecklistCount: toNumber(o.failedBarrierChecklistCount || o.FailedBarrierChecklistCount),
            closedCount: toNumber(o.closedCount || o.ClosedCount),
            registeredCount: toNumber(o.registeredCount || o.RegisteredCount),
            avgChecksRate: toNumber(o.avgChecksRate || o.AvgChecksRate),
            avgBarriersRate: toNumber(o.avgBarriersRate || o.AvgBarriersRate),
            refreshedAt: formatAnalyticsDateTime(o.refreshedAt || o.RefreshedAt || ""),
            source: String(o.source || o.Source || "ALL"),
            sourceText: String(o.sourceText || o.SourceText || o.source || o.Source || "All"),
            refreshState: normalizeRefreshState(o.refreshState),
            monthlyComparison: aMonthlyComparison,
            monthlyMatrixRows: aMonthlyMatrixRows,
            comparisonMetricSeries: mComparisonMetricSeries,
            comparisonChartRows: mComparisonMetricSeries.FAILED_CHECKS,
            charts: {
                failedChecksByProfession: aFailedChecksByProfession,
                failedBarriersByProfession: aFailedBarriersByProfession,
                failedChecksByLpc: aFailedChecksByLpc,
                failedBarriersByLpc: aFailedBarriersByLpc,
                failedChecksByLocation: aFailedChecksByLocation,
                failedBarriersByLocation: aFailedBarriersByLocation,
                failedChecksByBukrs: aFailedChecksByBukrs,
                failedBarriersByBukrs: aFailedBarriersByBukrs,
                totalBySource: aTotalBySource,
                failedChecksBySource: aFailedChecksBySource,
                failedBarriersBySource: aFailedBarriersBySource,
                failedBarriersByBarrierNumber: aFailedBarriersByBarrierNumber,
                monthlyTotal: normalizeChartRows(o.charts && o.charts.monthlyTotal),
                monthlyFailedChecks: normalizeChartRows(o.charts && o.charts.monthlyFailedChecks),
                monthlyFailedBarriers: normalizeChartRows(o.charts && o.charts.monthlyFailedBarriers),
                monthlyFailedChecklists: normalizeChartRows(o.charts && o.charts.monthlyFailedChecklists),
                monthlyFailedBarrierChecklists: normalizeChartRows(o.charts && o.charts.monthlyFailedBarrierChecklists)
            },
            hasCharts: {
                failedChecksByProfession: aFailedChecksByProfession.length > 0,
                failedBarriersByProfession: aFailedBarriersByProfession.length > 0,
                failedChecksByLpc: aFailedChecksByLpc.length > 0,
                failedBarriersByLpc: aFailedBarriersByLpc.length > 0,
                failedChecksByLocation: aFailedChecksByLocation.length > 0,
                failedBarriersByLocation: aFailedBarriersByLocation.length > 0,
                failedChecksByBukrs: aFailedChecksByBukrs.length > 0,
                failedBarriersByBukrs: aFailedBarriersByBukrs.length > 0,
                totalBySource: aTotalBySource.length > 0,
                failedChecksBySource: aFailedChecksBySource.length > 0,
                failedBarriersBySource: aFailedBarriersBySource.length > 0,
                failedBarriersByBarrierNumber: aFailedBarriersByBarrierNumber.length > 0,
                monthlyComparison: aMonthlyComparison.length > 0,
                comparisonChart: mComparisonMetricSeries.FAILED_CHECKS.length > 0
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
            failedChecklistCount: toNumber(o.failedChecklistCount || o.FailedChecklistCount),
            failedBarrierChecklistCount: toNumber(o.failedBarrierChecklistCount || o.FailedBarrierChecklistCount),
            avgChecksRate: toNumber(o.avgChecksRate || o.AvgChecksRate),
            avgBarriersRate: toNumber(o.avgBarriersRate || o.AvgBarriersRate),
            refreshedAtText: formatAnalyticsDateTime(o.refreshedAt || o.RefreshedAt || ""),
            sourceKey: String(o.source || o.Source || "ALL"),
            sourceText: String(o.sourceText || o.SourceText || o.source || o.Source || "All")
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
