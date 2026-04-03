sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/constants/AnalyticsContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/NullishPick",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, ODataAdapterUtils, AnalyticsContracts, NullishPick, GatewayContractConstants) {
    "use strict";

    function toNumber(vValue) {
        var nValue = Number(vValue);
        return isFinite(nValue) ? nValue : 0;
    }

    var firstDefined = NullishPick.firstDefined;

    function normalizeSummary(oData) {
        var oUnwrapped = ODataAdapterUtils.unwrap(oData);
        var o = Array.isArray(oUnwrapped) ? (oUnwrapped[0] || {}) : (oUnwrapped || {});
        var aAvailableYears = [];
        try {
            aAvailableYears = JSON.parse(String(o.availableYearsJson || o.AvailableYearsJson || "[]"));
        } catch (oError) {
            aAvailableYears = [];
        }
        return {
            selectedYear: toNumber(firstDefined(o.selectedYear, o.SelectedYear)),
            previousYear: toNumber(firstDefined(o.previousYear, o.PreviousYear)),
            availableYears: Array.isArray(aAvailableYears) ? aAvailableYears : [],
            total: toNumber(firstDefined(o.total, o.Total)),
            monthly: toNumber(firstDefined(o.monthly, o.Monthly)),
            failedChecks: toNumber(firstDefined(o.failedChecks, o.FailedChecks)),
            failedBarriers: toNumber(firstDefined(o.failedBarriers, o.FailedBarriers)),
            failedChecklistCount: toNumber(firstDefined(o.failedChecklistCount, o.FailedChecklistCount)),
            failedBarrierChecklistCount: toNumber(firstDefined(o.failedBarrierChecklistCount, o.FailedBarrierChecklistCount)),
            closedCount: toNumber(firstDefined(o.closedCount, o.ClosedCount)),
            registeredCount: toNumber(firstDefined(o.registeredCount, o.RegisteredCount)),
            avgChecksRate: toNumber(firstDefined(o.avgChecksRate, o.AvgChecksRate)),
            avgBarriersRate: toNumber(firstDefined(o.avgBarriersRate, o.AvgBarriersRate)),
            refreshedAt: String(firstDefined(o.refreshedAt, o.RefreshedAt, "-")),
            source: String(firstDefined(o.source, o.Source, AnalyticsContracts.SOURCES.ALL)),
            sourceText: String(firstDefined(o.sourceText, o.SourceText, o.source, o.Source, AnalyticsContracts.SOURCES.ALL))
        };
    }

    function normalizeChartRows(vRows) {
        return (Array.isArray(vRows) ? vRows : []).map(function (oRow, index) {
            return {
                label: String(firstDefined(oRow && oRow.label, oRow && oRow.Label, oRow && oRow.bucket_text, oRow && oRow.bucketText, "")),
                value: toNumber(firstDefined(oRow && oRow.value, oRow && oRow.Value, oRow && oRow.metric_value, oRow && oRow.metricValue)),
                order: toNumber(firstDefined(oRow && oRow.order, oRow && oRow.Order, index + 1))
            };
        });
    }

    function buildCharts(vRows) {
        var aRows = ODataAdapterUtils.asArray(vRows);
        var mTargetByPair = {
            PROFESSION_FAILED_CHECKS: "failedChecksByProfession",
            PROFESSION_FAILED_BARRIERS: "failedBarriersByProfession",
            LPC_FAILED_CHECKS: "failedChecksByLpc",
            LPC_FAILED_BARRIERS: "failedBarriersByLpc",
            LOCATION_FAILED_CHECKS: "failedChecksByLocation",
            LOCATION_FAILED_BARRIERS: "failedBarriersByLocation",
            BUKRS_FAILED_CHECKS: "failedChecksByBukrs",
            BUKRS_FAILED_BARRIERS: "failedBarriersByBukrs",
            ORGUNIT_FAILED_CHECKS: "failedChecksByOrgunit",
            ORGUNIT_FAILED_BARRIERS: "failedBarriersByOrgunit",
            SOURCE_TOTAL: "totalBySource",
            SOURCE_FAILED_CHECKS: "failedChecksBySource",
            SOURCE_FAILED_BARRIERS: "failedBarriersBySource",
            BARRIER_NUMBER_TOTAL: "totalBarriersByBarrierNumber",
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
            failedChecksByOrgunit: [],
            failedBarriersByOrgunit: [],
            totalBySource: [],
            failedChecksBySource: [],
            failedBarriersBySource: [],
            totalBarriersByBarrierNumber: [],
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
                    label: String(firstDefined(oRow && oRow.Label, oRow && oRow.label, "")),
                    value: toNumber(firstDefined(oRow && oRow.Value, oRow && oRow.value)),
                    order: toNumber(firstDefined(oRow && oRow.Order, oRow && oRow.order))
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
            failedChecksByOrgunit: normalizeChartRows(grouped.failedChecksByOrgunit),
            failedBarriersByOrgunit: normalizeChartRows(grouped.failedBarriersByOrgunit),
            totalBySource: normalizeChartRows(grouped.totalBySource),
            failedChecksBySource: normalizeChartRows(grouped.failedChecksBySource),
            failedBarriersBySource: normalizeChartRows(grouped.failedBarriersBySource),
            totalBarriersByBarrierNumber: normalizeChartRows(grouped.totalBarriersByBarrierNumber),
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

    function escapeFilterString(sValue) {
        return String(sValue || "").replace(/'/g, "''");
    }

    function buildBreakdownFilter(mInput) {
        var iSelectedYear = toNumber(mInput && mInput.selectedYear);
        var sSelectedSource = String(mInput && mInput.selectedSource || "").trim();
        var aParts = [];
        if (iSelectedYear > 0) {
            aParts.push("SelectedYear eq " + iSelectedYear);
        }
        if (sSelectedSource) {
            aParts.push("Source eq '" + escapeFilterString(sSelectedSource) + "'");
        }
        return aParts.join(" and ");
    }

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
        var oUnwrapped = ODataAdapterUtils.unwrap(oData);
            var o = Array.isArray(oUnwrapped) ? (oUnwrapped[0] || {}) : (oUnwrapped || {});
            return {
                taskKey: String(firstDefined(o.taskKey, o.TaskKey, AnalyticsContracts.REFRESH.TASK_KEY)),
                taskName: String(firstDefined(o.taskName, o.TaskName, AnalyticsContracts.REFRESH.TASK_NAME)),
                status: String(firstDefined(o.status, o.Status, AnalyticsContracts.REFRESH.STATUSES.IDLE)),
                isRunning: !!firstDefined(o.isRunning, o.IsRunning, false),
                requestedAt: String(firstDefined(o.requestedAt, o.RequestedAt, "")),
                requestedBy: String(firstDefined(o.requestedBy, o.RequestedBy, "")),
                startedAt: String(firstDefined(o.startedAt, o.StartedAt, "")),
                finishedAt: String(firstDefined(o.finishedAt, o.FinishedAt, "")),
                lastSuccessAt: String(firstDefined(o.lastSuccessAt, o.LastSuccessAt, "")),
                lastError: String(firstDefined(o.lastError, o.LastError, "")),
                lastMessage: String(firstDefined(o.lastMessage, o.LastMessage, "")),
                activeRunId: String(firstDefined(o.activeRunId, o.ActiveRunId, ""))
            };
        }

    function fetchSummary(mInput) {
        return GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.SIMPLE_ANALYTICAL, readParams(mInput), {
            responseGuardKey: "analytics.summary"
        }).then(normalizeSummary);
    }

    function fetchDetailed(mInput) {
                var mParams = readParams(mInput);
                var sBreakdownFilter = buildBreakdownFilter(mInput);
                var iSelectedYear = toNumber(mInput && mInput.selectedYear);
                var iCompareYear = resolveCompareYear(mInput, iSelectedYear);
                var sCompareBreakdownFilter = buildBreakdownFilter({
                    selectedYear: iCompareYear,
                    selectedSource: mInput && mInput.selectedSource
                });
                var pCompareBreakdown = iCompareYear === iSelectedYear
            ? GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.WORKFLOW_ANALYTICS_BREAKDOWN, { "$filter": sBreakdownFilter }, {
                        responseGuardKey: "analytics.breakdown.compare"
                    })
            : GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.WORKFLOW_ANALYTICS_BREAKDOWN, { "$filter": sCompareBreakdownFilter }, {
                        responseGuardKey: "analytics.breakdown.compare"
                    });
        var pRefreshState = GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.ANALYTICS_REFRESH_STATE + "('" + AnalyticsContracts.REFRESH.TASK_KEY + "')", {}, {
                    responseGuardKey: "analytics.refreshState"
                }).catch(function () { return null; });
                return Promise.all([
            GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.SIMPLE_ANALYTICAL, mParams, {
                        responseGuardKey: "analytics.summary"
                    }),
            GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.WORKFLOW_ANALYTICS_BREAKDOWN, { "$filter": sBreakdownFilter }, {
                        responseGuardKey: "analytics.breakdown.primary"
                    }),
                    pRefreshState,
                    pCompareBreakdown
                ]).then(function (aResult) {
                    return Object.assign({}, normalizeSummary(aResult[0]), {
                        charts: buildCharts(aResult[1]),
                        refreshState: normalizeRefreshState(aResult[2]),
                        compareYear: iCompareYear,
                        comparisonCharts: buildCharts(aResult[3])
                    });
                });
    }

    function fetchRefreshState() {
        return GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.ANALYTICS_REFRESH_STATE + "('" + AnalyticsContracts.REFRESH.TASK_KEY + "')", {}, {
            responseGuardKey: "analytics.refreshState"
        })
            .then(normalizeRefreshState)
            .catch(function () { return normalizeRefreshState(null); });
    }

    function requestRefresh(mInput) {
        return GatewayClient.callFunctionImport(GatewayContractConstants.FUNCTION_IMPORTS.ANALYTICS_REFRESH_TRIGGER, {
            TaskKey: AnalyticsContracts.REFRESH.TASK_KEY,
            RequestedBy: String(mInput && mInput.requestedBy || "")
        }).then(function () {
        return GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.ANALYTICS_REFRESH_STATE + "('" + AnalyticsContracts.REFRESH.TASK_KEY + "')", {}, {
                responseGuardKey: "analytics.refreshState"
            }).catch(function () { return null; });
        }).then(normalizeRefreshState);
    }

    return {
        fetchDetailed: fetchDetailed,
        fetchRefreshState: fetchRefreshState,
        fetchSummary: fetchSummary,
        requestRefresh: requestRefresh
    };
});
