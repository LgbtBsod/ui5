sap.ui.define([], function () {
    "use strict";

    function coerceBoolean(vRaw) {
        return vRaw === true
            || vRaw === "true"
            || vRaw === "TRUE"
            || vRaw === "X"
            || vRaw === 1
            || vRaw === "1";
    }

    function normalizeRows(oData) {
        if (Array.isArray(oData)) {
            return oData;
        }
        if (oData && Array.isArray(oData.results)) {
            return oData.results;
        }
        if (oData && Array.isArray(oData.value)) {
            return oData.value;
        }
        return [];
    }

    function resolveTotal(iTotalOverride, iVisible) {
        return Number.isFinite(iTotalOverride) && iTotalOverride >= 0 ? iTotalOverride : iVisible;
    }

    function buildKpis(aRows, iTotalOverride) {
        var aSafeRows = Array.isArray(aRows) ? aRows : [];
        var iVisible = aSafeRows.length;
        var iTotal = resolveTotal(iTotalOverride, iVisible);
        var iFailedChecks = 0;
        var iFailedBarriers = 0;
        var iHealthy = 0;

        aSafeRows.forEach(function (oRow) {
            var bFailedChecks = coerceBoolean(oRow && oRow.has_failed_checks);
            var bFailedBarriers = coerceBoolean(oRow && oRow.has_failed_barriers);
            if (bFailedChecks) {
                iFailedChecks += 1;
            }
            if (bFailedBarriers) {
                iFailedBarriers += 1;
            }
            if (!bFailedChecks && !bFailedBarriers) {
                iHealthy += 1;
            }
        });

        return {
            visible: iVisible,
            total: iTotal,
            failedChecks: iFailedChecks,
            failedBarriers: iFailedBarriers,
            healthy: iHealthy,
            workflowStage: iVisible > 0 ? "ANALYZE" : "DISCOVER"
        };
    }

    function buildMetrics(aRows, iTotalOverride) {
        var oKpi = buildKpis(aRows, iTotalOverride);
        return {
            kpi: oKpi,
            visible: oKpi.visible,
            total: oKpi.total
        };
    }

    return {
        normalizeRows: normalizeRows,
        resolveTotal: resolveTotal,
        buildKpis: buildKpis,
        buildMetrics: buildMetrics
    };
});
