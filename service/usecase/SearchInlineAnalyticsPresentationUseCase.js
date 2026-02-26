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

    function mapInlineAnalytics(mArgs) {
        var oAnalytics = (mArgs && mArgs.analytics) || {};
        var oBundle = mArgs && mArgs.bundle;

        var sSource = String(oAnalytics.source || "fallback").toLowerCase() === "backend" ? "backend" : "fallback";

        return {
            total: normalizeNumber(oAnalytics.total),
            monthly: normalizeNumber(oAnalytics.monthly),
            failedChecks: normalizeNumber(oAnalytics.failedChecks),
            failedBarriers: normalizeNumber(oAnalytics.failedBarriers),
            healthy: normalizeNumber(oAnalytics.healthy),
            avgChecksRate: normalizeNumber(oAnalytics.avgChecksRate),
            avgBarriersRate: normalizeNumber(oAnalytics.avgBarriersRate),
            refreshedAt: normalizeText(oAnalytics.refreshedAt, "-"),
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
