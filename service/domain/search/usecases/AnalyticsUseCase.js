sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/framework/Effects",
    "sap_ui5/service/framework/ComponentRuntimeSupport"
], function (UseCase, Result, Effects, ComponentRuntimeSupport) {
    "use strict";

    function AnalyticsUseCase() {
        UseCase.call(this, "AnalyticsUseCase");
    }

    AnalyticsUseCase.prototype = Object.create(UseCase.prototype);
    AnalyticsUseCase.prototype.constructor = AnalyticsUseCase;

    function formatAnalyticsDateTime(vDate) {
        if (vDate === null || vDate === undefined || vDate === "" || vDate === "-") {
            return "-";
        }
        return ComponentRuntimeSupport.formatHumanDateTime(vDate);
    }

    function normalizeChartRows(aRows) {
        var aSource = Array.isArray(aRows) ? aRows : [];
        var nMax = aSource.reduce(function (nAcc, oRow) {
            return Math.max(nAcc, Number(oRow && oRow.value) || 0);
        }, 0);
        return aSource.map(function (oRow, index) {
            var nValue = Number(oRow && oRow.value) || 0;
            var sLabel = String((oRow && oRow.label) || "");
            var nHeightRem = nMax > 0 ? Math.max(0.5, (nValue / nMax) * 7.5) : 0.5;
            return {
                label: sLabel,
                labelShort: sLabel.length > 12 ? sLabel.slice(0, 12) + "…" : sLabel,
                value: nValue,
                order: Number(oRow && oRow.order) || (index + 1),
                barHeight: nHeightRem.toFixed(2) + "rem"
            };
        });
    }

    function normalizeAnalytics(oSummary) {
        var o = oSummary || {};
        var aFailedChecksByProfession = normalizeChartRows(o.charts && o.charts.failedChecksByProfession);
        var aFailedBarriersByProfession = normalizeChartRows(o.charts && o.charts.failedBarriersByProfession);
        var aFailedChecksByLpc = normalizeChartRows(o.charts && o.charts.failedChecksByLpc);
        var aFailedBarriersByLpc = normalizeChartRows(o.charts && o.charts.failedBarriersByLpc);
        return {
            total: Number(o.total || o.Total || 0),
            failedChecks: Number(o.failedChecks || o.FailedChecks || 0),
            failedBarriers: Number(o.failedBarriers || o.FailedBarriers || 0),
            closedCount: Number(o.closedCount || o.ClosedCount || 0),
            registeredCount: Number(o.registeredCount || o.RegisteredCount || 0),
            avgChecksRate: Number(o.avgChecksRate || o.AvgChecksRate || 0),
            avgBarriersRate: Number(o.avgBarriersRate || o.AvgBarriersRate || 0),
            refreshedAt: formatAnalyticsDateTime(o.refreshedAt || o.RefreshedAt || ""),
            source: String(o.source || o.Source || "backend"),
            charts: {
                failedChecksByProfession: aFailedChecksByProfession,
                failedBarriersByProfession: aFailedBarriersByProfession,
                failedChecksByLpc: aFailedChecksByLpc,
                failedBarriersByLpc: aFailedBarriersByLpc
            },
            hasCharts: {
                failedChecksByProfession: aFailedChecksByProfession.length > 0,
                failedBarriersByProfession: aFailedBarriersByProfession.length > 0,
                failedChecksByLpc: aFailedChecksByLpc.length > 0,
                failedBarriersByLpc: aFailedBarriersByLpc.length > 0
            }
        };
    }

    function buildRailPayload(oSummary) {
        var o = oSummary || {};
        return {
            total: Number(o.total || o.Total || 0),
            monthly: Number(o.monthly || o.Monthly || 0),
            failedChecks: Number(o.failedChecks || o.FailedChecks || 0),
            failedBarriers: Number(o.failedBarriers || o.FailedBarriers || 0),
            avgChecksRate: Number(o.avgChecksRate || o.AvgChecksRate || 0),
            avgBarriersRate: Number(o.avgBarriersRate || o.AvgBarriersRate || 0),
            refreshedAtText: formatAnalyticsDateTime(o.refreshedAt || o.RefreshedAt || ""),
            sourceText: String(o.source || o.Source || "backend")
        };
    }

    AnalyticsUseCase.prototype.execute = function (mInput, mCtx) {
        var sIntent = (mInput && mInput.intent) || "";
        var bSilent = !!(mInput && mInput.silent);
        var bUserInitiated = !!(mInput && mInput.userInitiated);
        if (!sIntent) {
            return Promise.resolve(Result.ok({}, []));
        }
        if (sIntent === "closeDialog") {
            return Promise.resolve(Result.ok({}, [
                Effects.modelPatch("view", "/analyticsBusy", false),
                Effects.dialog("workflowAnalytics", "close", {})
            ]));
        }
        var oAnalytics = mCtx && mCtx.analytics;
        if (sIntent === "refreshRail") {
            var pSummary = (oAnalytics && typeof oAnalytics.fetchSummary === "function")
                ? oAnalytics.fetchSummary()
                : Promise.resolve(buildRailPayload({}));
            return Promise.resolve(pSummary).then(function (oSummary) {
                var aEffects = [
                    Effects.modelPatch("view", "/analyticsError", ""),
                    Effects.modelPatch("view", "/analyticsRail", buildRailPayload(oSummary))
                ];
                if (!bSilent) {
                    aEffects.unshift(Effects.modelPatch("view", "/analyticsRailBusy", false));
                }
                return Result.ok({ analyticsRail: oSummary || {} }, aEffects);
            }).catch(function (oError) {
                var aEffects = [
                    Effects.modelPatch("view", "/analyticsError", String((oError && oError.message) || "Analytics unavailable"))
                ];
                if (!bSilent) {
                    aEffects.unshift(Effects.modelPatch("view", "/analyticsRailBusy", false));
                }
                return Result.fail(oError, aEffects);
            });
        }
        if (sIntent !== "openDialog" || !bUserInitiated) {
            return Promise.resolve(Result.ok({}, [
                Effects.modelPatch("view", "/analyticsBusy", false)
            ]));
        }
        var pSummary = (oAnalytics && typeof oAnalytics.fetchDetailed === "function")
            ? oAnalytics.fetchDetailed()
            : Promise.resolve(normalizeAnalytics({}));
        return Promise.resolve(pSummary).then(function (oSummary) {
            var mAnalytics = normalizeAnalytics(oSummary);
            return Result.ok({ analytics: mAnalytics }, [
                Effects.modelPatch("view", "/analyticsBusy", false),
                Effects.modelPatch("view", "/analyticsError", ""),
                Effects.modelPatch("view", "/analytics", mAnalytics)
            ]);
        }).catch(function (oError) {
            return Result.fail(oError, [
                Effects.modelPatch("view", "/analyticsBusy", false),
                Effects.modelPatch("view", "/analyticsError", String((oError && oError.message) || "Analytics unavailable"))
            ]);
        });
    };

    return AnalyticsUseCase;
});
