sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFormattingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsChartValueRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsMonthlyComparisonRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsKpiDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsRefreshStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/NullishPick"
], function (ComponentFormattingRuntime, AnalyticsContracts, AnalyticsChartValueRuntime, AnalyticsMonthlyComparisonRuntime, AnalyticsKpiDeltaRuntime, AnalyticsRefreshStateRuntime, NullishPick) {
    "use strict";

    function formatAnalyticsDateTime(vDate) {
        if (vDate === null || vDate === undefined || vDate === "" || vDate === "-") {
            return "-";
        }
        return ComponentFormattingRuntime.formatHumanDateTime(vDate);
    }

    var firstDefined = NullishPick.firstDefined;

    function buildAvailableYears(vYears, iSelectedYear, iCompareYear, iPreviousYear) {
        var mSeen = {};
        var aItems = [];

        function pushYear(vYear) {
            var iYear = AnalyticsChartValueRuntime.toNumber(vYear);
            var sYear = iYear > 0 ? String(iYear) : String(vYear || "").trim();
            if (!/^\d{4}$/.test(sYear) || mSeen[sYear]) {
                return;
            }
            mSeen[sYear] = true;
            aItems.push({
                key: sYear,
                text: sYear
            });
        }

        (Array.isArray(vYears) ? vYears : []).forEach(function (oYear) {
            pushYear((oYear && (oYear.key || oYear.text || oYear.year || oYear.Year)) || "");
        });
        [iSelectedYear, iCompareYear, iPreviousYear].forEach(pushYear);

        return aItems.sort(function (aLeft, aRight) {
            return AnalyticsChartValueRuntime.toNumber(aRight && aRight.key) - AnalyticsChartValueRuntime.toNumber(aLeft && aLeft.key);
        });
    }

    function normalizeDashboard(oSummary) {
        var o = oSummary || {};
        var aFailedChecksByProfession = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedChecksByProfession);
        var aFailedBarriersByProfession = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedBarriersByProfession);
        var aFailedChecksByLpc = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedChecksByLpc);
        var aFailedBarriersByLpc = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedBarriersByLpc);
        var aFailedChecksByLocation = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedChecksByLocation);
        var aFailedBarriersByLocation = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedBarriersByLocation);
        var aFailedChecksByBukrs = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedChecksByBukrs);
        var aFailedBarriersByBukrs = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedBarriersByBukrs);
        var aFailedChecksByOrgunit = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedChecksByOrgunit);
        var aFailedBarriersByOrgunit = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedBarriersByOrgunit);
        var aTotalBySource = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.totalBySource);
        var aFailedChecksBySource = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedChecksBySource);
        var aFailedBarriersBySource = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedBarriersBySource);
        var aTotalBarriersByBarrierNumber = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.totalBarriersByBarrierNumber);
        var aFailedBarriersByBarrierNumber = AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.failedBarriersByBarrierNumber);
        var iSelectedYear = AnalyticsChartValueRuntime.toNumber(firstDefined(o.selectedYear, o.SelectedYear));
        var iCompareYear = AnalyticsChartValueRuntime.toNumber(firstDefined(o.compareYear, o.CompareYear, o.previousYear, o.PreviousYear));
        var aMonthlyComparison = AnalyticsMonthlyComparisonRuntime.buildMonthlyComparison(o.charts || {}, o.comparisonCharts || {});
        var mKpiDeltas = AnalyticsKpiDeltaRuntime.buildKpiDeltas({
            total: AnalyticsChartValueRuntime.toNumber(firstDefined(o.total, o.Total)),
            failedChecks: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedChecks, o.FailedChecks)),
            failedBarriers: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedBarriers, o.FailedBarriers)),
            failedChecklistCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedChecklistCount, o.FailedChecklistCount)),
            failedBarrierChecklistCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedBarrierChecklistCount, o.FailedBarrierChecklistCount))
        }, aMonthlyComparison);
        var mComparisonMetricSeries = {
            TOTAL: AnalyticsMonthlyComparisonRuntime.buildMetricSeriesRows(aMonthlyComparison, "TOTAL"),
            FAILED_CHECKS: AnalyticsMonthlyComparisonRuntime.buildMetricSeriesRows(aMonthlyComparison, "FAILED_CHECKS"),
            FAILED_BARRIERS: AnalyticsMonthlyComparisonRuntime.buildMetricSeriesRows(aMonthlyComparison, "FAILED_BARRIERS"),
            FAILED_CHECKLISTS: AnalyticsMonthlyComparisonRuntime.buildMetricSeriesRows(aMonthlyComparison, "FAILED_CHECKLISTS"),
            FAILED_BARRIER_CHECKLISTS: AnalyticsMonthlyComparisonRuntime.buildMetricSeriesRows(aMonthlyComparison, "FAILED_BARRIER_CHECKLISTS")
        };
        var aMonthlyMatrixRows = AnalyticsMonthlyComparisonRuntime.buildMonthlyMatrixRows(aMonthlyComparison, iSelectedYear, iCompareYear);
        var iPreviousYear = AnalyticsChartValueRuntime.toNumber(firstDefined(o.previousYear, o.PreviousYear));
        var aAvailableYears = buildAvailableYears(o.availableYears, iSelectedYear, iCompareYear, iPreviousYear);

        return {
            selectedYear: iSelectedYear,
            previousYear: iPreviousYear,
            compareYear: iCompareYear,
            availableYears: aAvailableYears,
            total: AnalyticsChartValueRuntime.toNumber(firstDefined(o.total, o.Total)),
            failedChecks: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedChecks, o.FailedChecks)),
            failedBarriers: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedBarriers, o.FailedBarriers)),
            failedChecklistCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedChecklistCount, o.FailedChecklistCount)),
            failedBarrierChecklistCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedBarrierChecklistCount, o.FailedBarrierChecklistCount)),
            closedCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.closedCount, o.ClosedCount)),
            registeredCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.registeredCount, o.RegisteredCount)),
            avgChecksRate: AnalyticsChartValueRuntime.toNumber(firstDefined(o.avgChecksRate, o.AvgChecksRate)),
            avgBarriersRate: AnalyticsChartValueRuntime.toNumber(firstDefined(o.avgBarriersRate, o.AvgBarriersRate)),
            refreshedAt: formatAnalyticsDateTime(firstDefined(o.refreshedAt, o.RefreshedAt, "")),
            source: String(firstDefined(o.source, o.Source, AnalyticsContracts.SOURCES.ALL)),
            sourceText: String(firstDefined(o.sourceText, o.SourceText, o.source, o.Source, "All")),
            compareYearHasData: AnalyticsChartValueRuntime.hasAnyChartValue(o.comparisonCharts && o.comparisonCharts.monthlyTotal),
            refreshState: AnalyticsRefreshStateRuntime.normalizeRefreshState(o.refreshState, formatAnalyticsDateTime),
            monthlyComparison: aMonthlyComparison,
            monthlyMatrixRows: aMonthlyMatrixRows,
            deltas: mKpiDeltas,
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
                failedChecksByOrgunit: aFailedChecksByOrgunit,
                failedBarriersByOrgunit: aFailedBarriersByOrgunit,
                totalBySource: aTotalBySource,
                failedChecksBySource: aFailedChecksBySource,
                failedBarriersBySource: aFailedBarriersBySource,
                totalBarriersByBarrierNumber: aTotalBarriersByBarrierNumber,
                failedBarriersByBarrierNumber: aFailedBarriersByBarrierNumber,
                monthlyTotal: AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.monthlyTotal),
                monthlyFailedChecks: AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.monthlyFailedChecks),
                monthlyFailedBarriers: AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.monthlyFailedBarriers),
                monthlyFailedChecklists: AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.monthlyFailedChecklists),
                monthlyFailedBarrierChecklists: AnalyticsChartValueRuntime.normalizeChartRows(o.charts && o.charts.monthlyFailedBarrierChecklists)
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
                failedChecksByOrgunit: aFailedChecksByOrgunit.length > 0,
                failedBarriersByOrgunit: aFailedBarriersByOrgunit.length > 0,
                totalBySource: aTotalBySource.length > 0,
                failedChecksBySource: aFailedChecksBySource.length > 0,
                failedBarriersBySource: aFailedBarriersBySource.length > 0,
                totalBarriersByBarrierNumber: aTotalBarriersByBarrierNumber.length > 0,
                failedBarriersByBarrierNumber: aFailedBarriersByBarrierNumber.length > 0,
                monthlyComparison: AnalyticsChartValueRuntime.hasAnySeriesValue(aMonthlyComparison),
                comparisonChart: AnalyticsChartValueRuntime.hasAnySeriesValue(mComparisonMetricSeries.FAILED_CHECKS)
            }
        };
    }

    function buildRailPayload(oSummary) {
        var o = oSummary || {};

        return {
            selectedYear: AnalyticsChartValueRuntime.toNumber(firstDefined(o.selectedYear, o.SelectedYear)),
            previousYear: AnalyticsChartValueRuntime.toNumber(firstDefined(o.previousYear, o.PreviousYear)),
            total: AnalyticsChartValueRuntime.toNumber(firstDefined(o.total, o.Total)),
            monthly: AnalyticsChartValueRuntime.toNumber(firstDefined(o.monthly, o.Monthly)),
            failedChecks: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedChecks, o.FailedChecks)),
            failedBarriers: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedBarriers, o.FailedBarriers)),
            failedChecklistCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedChecklistCount, o.FailedChecklistCount)),
            failedBarrierChecklistCount: AnalyticsChartValueRuntime.toNumber(firstDefined(o.failedBarrierChecklistCount, o.FailedBarrierChecklistCount)),
            avgChecksRate: AnalyticsChartValueRuntime.toNumber(firstDefined(o.avgChecksRate, o.AvgChecksRate)),
            avgBarriersRate: AnalyticsChartValueRuntime.toNumber(firstDefined(o.avgBarriersRate, o.AvgBarriersRate)),
            refreshedAtText: formatAnalyticsDateTime(firstDefined(o.refreshedAt, o.RefreshedAt, "")),
            sourceKey: String(firstDefined(o.source, o.Source, AnalyticsContracts.SOURCES.ALL)),
            sourceText: String(firstDefined(o.sourceText, o.SourceText, o.source, o.Source, "All"))
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
