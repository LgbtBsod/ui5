sap.ui.define([
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/service/SmartSearchAdapter"
], function (BackendAdapter, SmartSearchAdapter) {
    "use strict";

    function _safeNumber(v, iFallback) {
        var n = Number(v);
        return Number.isFinite(n) ? n : (iFallback || 0);
    }


    function _extractBackendPayload(oData) {
        if (oData && Array.isArray(oData.value) && oData.value.length) {
            return oData.value[0] || {};
        }
        return oData || {};
    }

    function _buildFromCollection(aRows) {
        var a = Array.isArray(aRows) ? aRows : [];
        var iTotal = a.length;
        var sMonthPrefix = new Date().toISOString().slice(0, 7);
        var iMonthly = 0;
        var iFailedChecks = 0;
        var iFailedBarriers = 0;
        var iClosed = 0;
        var iRegistered = 0;
        var nChecksRate = 0;
        var nBarriersRate = 0;

        a.forEach(function (oItem) {
            var oRoot = (oItem && oItem.root) || {};
            if (oRoot.has_failed_checks === true || oRoot.hasFailedChecks === true) {
                iFailedChecks += 1;
            }
            if (oRoot.has_failed_barriers === true || oRoot.hasFailedBarriers === true) {
                iFailedBarriers += 1;
            }
            var sDate = String((oItem && oItem.basic && oItem.basic.date) || oRoot.date || "");
            if (sDate.slice(0, 7) === sMonthPrefix) {
                iMonthly += 1;
            }
            if (String(oRoot.status || "").toUpperCase() === "CLOSED") {
                iClosed += 1;
            }
            if (String(oRoot.status || "").toUpperCase() === "REGISTERED") {
                iRegistered += 1;
            }
            nChecksRate += _safeNumber(oRoot.successRateChecks, 0);
            nBarriersRate += _safeNumber(oRoot.successRateBarriers, 0);
        });

        var nAvgChecks = iTotal ? Math.round(nChecksRate / iTotal) : 0;
        var nAvgBarriers = iTotal ? Math.round(nBarriersRate / iTotal) : 0;

        return {
            total: iTotal,
            monthly: iMonthly,
            failedChecks: iFailedChecks,
            failedBarriers: iFailedBarriers,
            healthy: Math.max(0, iTotal - Math.max(iFailedChecks, iFailedBarriers)),
            closedCount: iClosed,
            registeredCount: iRegistered,
            avgChecksRate: nAvgChecks,
            avgBarriersRate: nAvgBarriers,
            refreshedAt: new Date().toISOString(),
            source: "fallback"
        };
    }

    return {
        loadProcessAnalytics: function (mPayload, sSearchMode, aFallbackCollection) {
            var pBackend = (BackendAdapter.getProcessAnalytics
                ? BackendAdapter.getProcessAnalytics(mPayload || {}, sSearchMode || "EXACT")
                : Promise.reject(new Error("Analytics API unavailable")));

            return pBackend.then(function (oData) {
                var oPayload = _extractBackendPayload(oData);
                return {
                    total: _safeNumber(oPayload.total || oPayload.TOTAL, 0),
                    monthly: _safeNumber(oPayload.monthly || oPayload.MONTHLY, 0),
                    failedChecks: _safeNumber(oPayload.failedChecks || oPayload.FAILED_CHECKS, 0),
                    failedBarriers: _safeNumber(oPayload.failedBarriers || oPayload.FAILED_BARRIERS, 0),
                    healthy: _safeNumber(oPayload.healthy || oPayload.HEALTHY, 0),
                    closedCount: _safeNumber(oPayload.closedCount || oPayload.CLOSED_COUNT, 0),
                    registeredCount: _safeNumber(oPayload.registeredCount || oPayload.REGISTERED_COUNT, 0),
                    avgChecksRate: _safeNumber(oPayload.avgChecksRate || oPayload.AVG_CHECKS_RATE || oPayload.failedChecksRate || oPayload.FAILED_CHECKS_RATE || oPayload.failed_checks_pct || oPayload.FAILED_CHECKS_PCT, 0),
                    avgBarriersRate: _safeNumber(oPayload.avgBarriersRate || oPayload.AVG_BARRIERS_RATE || oPayload.failedBarriersRate || oPayload.FAILED_BARRIERS_RATE || oPayload.failed_barriers_pct || oPayload.FAILED_BARRIERS_PCT, 0),
                    refreshedAt: (oPayload.refreshedAt || oPayload.REFRESHED_AT) || new Date().toISOString(),
                    source: "backend"
                };
            }).catch(function () {
                var aFiltered = SmartSearchAdapter.filterData(aFallbackCollection || [], mPayload || {}, sSearchMode || "EXACT");
                return _buildFromCollection(aFiltered);
            });
        },

        loadSimpleAnalytics: function (aFallbackCollection) {
            var pBackend = (BackendAdapter.getSimpleAnalytics
                ? BackendAdapter.getSimpleAnalytics()
                : Promise.reject(new Error("Simple analytics API unavailable")));

            return pBackend.then(function (oData) {
                var oPayload = _extractBackendPayload(oData);
                return {
                    total: _safeNumber(oPayload.total || oPayload.TOTAL, 0),
                    monthly: _safeNumber(oPayload.monthly || oPayload.MONTHLY, 0),
                    failedChecks: _safeNumber(oPayload.failedChecks || oPayload.FAILED_CHECKS, 0),
                    failedBarriers: _safeNumber(oPayload.failedBarriers || oPayload.FAILED_BARRIERS, 0),
                    healthy: _safeNumber(oPayload.healthy || oPayload.HEALTHY, 0),
                    closedCount: _safeNumber(oPayload.closedCount || oPayload.CLOSED_COUNT, 0),
                    registeredCount: _safeNumber(oPayload.registeredCount || oPayload.REGISTERED_COUNT, 0),
                    avgChecksRate: _safeNumber(oPayload.avgChecksRate || oPayload.AVG_CHECKS_RATE || oPayload.failedChecksRate || oPayload.FAILED_CHECKS_RATE || oPayload.failed_checks_pct || oPayload.FAILED_CHECKS_PCT, 0),
                    avgBarriersRate: _safeNumber(oPayload.avgBarriersRate || oPayload.AVG_BARRIERS_RATE || oPayload.failedBarriersRate || oPayload.FAILED_BARRIERS_RATE || oPayload.failed_barriers_pct || oPayload.FAILED_BARRIERS_PCT, 0),
                    refreshedAt: (oPayload.refreshedAt || oPayload.REFRESHED_AT) || new Date().toISOString(),
                    source: "backend"
                };
            }).catch(function () {
                return _buildFromCollection(aFallbackCollection || []);
            });

        }
    };
});
