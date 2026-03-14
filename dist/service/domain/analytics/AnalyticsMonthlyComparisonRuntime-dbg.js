sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsChartValueRuntime"
], function (AnalyticsChartValueRuntime) {
    "use strict";

    var MONTH_LABELS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    var METRIC_MAP = {
        TOTAL: ["selectedTotal", "compareTotal"],
        FAILED_CHECKS: ["selectedFailedChecks", "compareFailedChecks"],
        FAILED_BARRIERS: ["selectedFailedBarriers", "compareFailedBarriers"],
        FAILED_CHECKLISTS: ["selectedFailedChecklists", "compareFailedChecklists"],
        FAILED_BARRIER_CHECKLISTS: ["selectedFailedBarrierChecklists", "compareFailedBarrierChecklists"]
    };

    function buildMonthlyComparison(oSelectedCharts, oCompareCharts) {
        var mSelectedTotal = AnalyticsChartValueRuntime.toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyTotal);
        var mCompareTotal = AnalyticsChartValueRuntime.toMonthlyMap(oCompareCharts && oCompareCharts.monthlyTotal);
        var mSelectedChecks = AnalyticsChartValueRuntime.toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedChecks);
        var mCompareChecks = AnalyticsChartValueRuntime.toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedChecks);
        var mSelectedBarriers = AnalyticsChartValueRuntime.toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedBarriers);
        var mCompareBarriers = AnalyticsChartValueRuntime.toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedBarriers);
        var mSelectedFailedChecklists = AnalyticsChartValueRuntime.toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedChecklists);
        var mCompareFailedChecklists = AnalyticsChartValueRuntime.toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedChecklists);
        var mSelectedFailedBarrierChecklists = AnalyticsChartValueRuntime.toMonthlyMap(oSelectedCharts && oSelectedCharts.monthlyFailedBarrierChecklists);
        var mCompareFailedBarrierChecklists = AnalyticsChartValueRuntime.toMonthlyMap(oCompareCharts && oCompareCharts.monthlyFailedBarrierChecklists);

        return Array.apply(null, new Array(12)).map(function (_vUnused, index) {
            var iMonth = index + 1;
            return {
                monthIndex: iMonth,
                monthLabel: MONTH_LABELS[index],
                selectedTotal: AnalyticsChartValueRuntime.toNumber(mSelectedTotal[iMonth]),
                compareTotal: AnalyticsChartValueRuntime.toNumber(mCompareTotal[iMonth]),
                selectedFailedChecks: AnalyticsChartValueRuntime.toNumber(mSelectedChecks[iMonth]),
                compareFailedChecks: AnalyticsChartValueRuntime.toNumber(mCompareChecks[iMonth]),
                selectedFailedBarriers: AnalyticsChartValueRuntime.toNumber(mSelectedBarriers[iMonth]),
                compareFailedBarriers: AnalyticsChartValueRuntime.toNumber(mCompareBarriers[iMonth]),
                selectedFailedChecklists: AnalyticsChartValueRuntime.toNumber(mSelectedFailedChecklists[iMonth]),
                compareFailedChecklists: AnalyticsChartValueRuntime.toNumber(mCompareFailedChecklists[iMonth]),
                selectedFailedBarrierChecklists: AnalyticsChartValueRuntime.toNumber(mSelectedFailedBarrierChecklists[iMonth]),
                compareFailedBarrierChecklists: AnalyticsChartValueRuntime.toNumber(mCompareFailedBarrierChecklists[iMonth])
            };
        });
    }

    function buildMetricSeriesRows(aRows, sMetricKey) {
        return (Array.isArray(aRows) ? aRows : []).map(function (oRow) {
            var aProps = METRIC_MAP[sMetricKey] || METRIC_MAP.FAILED_CHECKS;
            return {
                monthLabel: String(oRow && oRow.monthLabel || ""),
                selectedValue: AnalyticsChartValueRuntime.toNumber(oRow && oRow[aProps[0]]),
                compareValue: AnalyticsChartValueRuntime.toNumber(oRow && oRow[aProps[1]])
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
                var sMonthProp = AnalyticsChartValueRuntime.monthProperty(AnalyticsChartValueRuntime.toNumber(oRow && oRow.monthIndex));
                oSelectedRow[sMonthProp] = AnalyticsChartValueRuntime.toNumber(oRow && oRow[oMetric.selectedProp]);
                oCompareRow[sMonthProp] = AnalyticsChartValueRuntime.toNumber(oRow && oRow[oMetric.compareProp]);
            });
            aResult.push(oSelectedRow, oCompareRow);
            return aResult;
        }, []);
    }

    return {
        buildMetricSeriesRows: buildMetricSeriesRows,
        buildMonthlyComparison: buildMonthlyComparison,
        buildMonthlyMatrixRows: buildMonthlyMatrixRows
    };
});
