sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/analytics/AnalyticsChartValueRuntime"
], function (AnalyticsChartValueRuntime) {
    "use strict";

    function formatSignedValue(nValue) {
        var nRounded = Math.round(AnalyticsChartValueRuntime.toNumber(nValue));
        if (nRounded > 0) {
            return "+" + String(nRounded);
        }
        return String(nRounded);
    }

    function buildDeltaState(nValue, bPositiveIsGood) {
        var nSafe = AnalyticsChartValueRuntime.toNumber(nValue);
        if (nSafe === 0) {
            return "None";
        }
        if (bPositiveIsGood) {
            return nSafe > 0 ? "Success" : "Error";
        }
        return nSafe < 0 ? "Success" : "Error";
    }

    function buildKpiDeltas(oDashboard, aMonthlyComparison) {
        var aRows = Array.isArray(aMonthlyComparison) ? aMonthlyComparison : [];

        function sumBy(sProp) {
            return aRows.reduce(function (nAcc, oRow) {
                return nAcc + AnalyticsChartValueRuntime.toNumber(oRow && oRow[sProp]);
            }, 0);
        }

        function buildDelta(nSelected, nCompare, bPositiveIsGood) {
            var nValue = AnalyticsChartValueRuntime.toNumber(nSelected) - AnalyticsChartValueRuntime.toNumber(nCompare);
            return {
                value: nValue,
                text: formatSignedValue(nValue),
                state: buildDeltaState(nValue, bPositiveIsGood)
            };
        }

        return {
            total: buildDelta(oDashboard.total, sumBy("compareTotal"), true),
            failedChecks: buildDelta(oDashboard.failedChecks, sumBy("compareFailedChecks"), false),
            failedBarriers: buildDelta(oDashboard.failedBarriers, sumBy("compareFailedBarriers"), false),
            failedChecklistCount: buildDelta(oDashboard.failedChecklistCount, sumBy("compareFailedChecklists"), false),
            failedBarrierChecklistCount: buildDelta(oDashboard.failedBarrierChecklistCount, sumBy("compareFailedBarrierChecklists"), false)
        };
    }

    return {
        buildKpiDeltas: buildKpiDeltas
    };
});
