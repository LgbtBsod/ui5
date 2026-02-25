sap.ui.define([], function () {
    "use strict";

    function normalizeCount(vCount) {
        var iValue = Number(vCount);
        if (!Number.isFinite(iValue) || iValue < 0) {
            return 0;
        }
        return Math.floor(iValue);
    }

    function normalizeStage(sStage) {
        var sValue = String(sStage || "").trim().toUpperCase();
        if (["DISCOVER", "ANALYZE", "REVIEW"].indexOf(sValue) < 0) {
            return "DISCOVER";
        }
        return sValue;
    }

    function buildResultSummaryText(mArgs) {
        var oBundle = mArgs && mArgs.bundle;
        var iVisible = normalizeCount(mArgs && mArgs.visible);
        var iTotal = normalizeCount(mArgs && mArgs.total);

        if (oBundle && typeof oBundle.getText === "function") {
            try {
                return oBundle.getText("resultSummary", [iVisible, iTotal]);
            } catch (_e) {
                return iVisible + " of " + iTotal;
            }
        }

        return iVisible + " of " + iTotal;
    }

    function buildSummaryPresentation(mArgs) {
        var oKpi = (mArgs && mArgs.kpi) || {};
        return {
            kpiVisible: normalizeCount(oKpi.visible),
            kpiTotal: normalizeCount(oKpi.total),
            kpiFailedChecks: normalizeCount(oKpi.failedChecks),
            kpiFailedBarriers: normalizeCount(oKpi.failedBarriers),
            kpiHealthy: normalizeCount(oKpi.healthy),
            workflowStage: normalizeStage(oKpi.workflowStage),
            resultSummary: buildResultSummaryText({
                bundle: mArgs && mArgs.bundle,
                visible: mArgs && mArgs.visible,
                total: mArgs && mArgs.total
            }),
            lastUpdatedAt: (mArgs && mArgs.lastUpdatedAt) || new Date().toLocaleTimeString()
        };
    }

    function applySummaryPresentation(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return { ok: false, reason: "missing_view_model" };
        }

        var oPresentation = buildSummaryPresentation(mArgs || {});

        oViewModel.setProperty("/kpiVisible", oPresentation.kpiVisible);
        oViewModel.setProperty("/kpiTotal", oPresentation.kpiTotal);
        oViewModel.setProperty("/kpiFailedChecks", oPresentation.kpiFailedChecks);
        oViewModel.setProperty("/kpiFailedBarriers", oPresentation.kpiFailedBarriers);
        oViewModel.setProperty("/kpiHealthy", oPresentation.kpiHealthy);
        oViewModel.setProperty("/workflowStage", oPresentation.workflowStage);
        oViewModel.setProperty("/resultSummary", oPresentation.resultSummary);
        oViewModel.setProperty("/lastUpdatedAt", oPresentation.lastUpdatedAt);

        return {
            ok: true,
            reason: "applied",
            presentation: oPresentation
        };
    }

    return {
        normalizeCount: normalizeCount,
        normalizeStage: normalizeStage,
        buildResultSummaryText: buildResultSummaryText,
        buildSummaryPresentation: buildSummaryPresentation,
        applySummaryPresentation: applySummaryPresentation
    };
});
