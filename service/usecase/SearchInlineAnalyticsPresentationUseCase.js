sap.ui.define([], function () {
    "use strict";

    function normalizeNumber(v) {
        var n = Number(v);
        return Number.isFinite(n) ? n : 0;
    }

    function normalizeText(v, sFallback) {
        var s = String(v || "").trim();
        return s || (sFallback || "-");
    }

    function resolveSourceText(oBundle, sSource) {
        var sKey = sSource === "backend" ? "analyticsSourceBackend" : "analyticsSourceFallback";
        if (!oBundle || typeof oBundle.getText !== "function") {
            return sSource || "fallback";
        }
        try {
            return oBundle.getText(sKey);
        } catch (e) {
            return sSource || "fallback";
        }
    }

    function formatUpdatedDate(sIso) {
        if (!sIso) {
            return "-";
        }
        var oDate = new Date(sIso);
        if (Number.isNaN(oDate.getTime())) {
            return normalizeText(sIso, "-");
        }
        return oDate.toLocaleDateString(undefined, {
            weekday: "short",
            day: "2-digit",
            month: "short",
            year: "numeric"
        });
    }

    function normalizeRate(vRate, iTotal, iFailed) {
        var nRate = Number(vRate);
        if (Number.isFinite(nRate)) {
            return Math.max(0, Math.min(100, Math.round(nRate)));
        }
        if (iTotal <= 0) {
            return 0;
        }
        return Math.max(0, Math.min(100, Math.round((iFailed / iTotal) * 100)));
    }

    function mapInlineAnalytics(mArgs) {
        var oAnalytics = (mArgs && mArgs.analytics) || {};
        var oBundle = mArgs && mArgs.bundle;

        var sSource = String(oAnalytics.source || "fallback").toLowerCase() === "backend" ? "backend" : "fallback";
        var iTotal = normalizeNumber(oAnalytics.total);
        var iMonthly = normalizeNumber(oAnalytics.monthly);
        var iFailedChecks = normalizeNumber(oAnalytics.failedChecks);
        var iFailedBarriers = normalizeNumber(oAnalytics.failedBarriers);

        return {
            total: iTotal,
            monthly: iMonthly,
            failedChecks: iFailedChecks,
            failedBarriers: iFailedBarriers,
            healthy: normalizeNumber(oAnalytics.healthy),
            avgChecksRate: normalizeRate(oAnalytics.avgChecksRate, iMonthly || iTotal, iFailedChecks),
            avgBarriersRate: normalizeRate(oAnalytics.avgBarriersRate, iMonthly || iTotal, iFailedBarriers),
            refreshedAt: normalizeText(oAnalytics.refreshedAt, "-"),
            refreshedAtText: formatUpdatedDate(oAnalytics.refreshedAt),
            source: sSource,
            sourceText: resolveSourceText(oBundle, sSource)
        };
    }

    function applyInlineAnalyticsPresentation(mArgs) {
        var oViewModel = mArgs && mArgs.viewModel;
        if (!oViewModel || typeof oViewModel.setProperty !== "function") {
            return null;
        }

        var oMapped = mapInlineAnalytics(mArgs);
        oViewModel.setProperty("/analyticsRail", oMapped);
        return oMapped;
    }

    return {
        normalizeNumber: normalizeNumber,
        normalizeText: normalizeText,
        resolveSourceText: resolveSourceText,
        mapInlineAnalytics: mapInlineAnalytics,
        applyInlineAnalyticsPresentation: applyInlineAnalyticsPresentation
    };
});
