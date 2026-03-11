sap.ui.define([], function () {
    "use strict";

    var DEFAULT_EXPORT_LIMIT = 200000;

    function resolvePositiveInt(vValue, iMax) {
        var sValue = String(vValue || "").trim();
        var iParsed = Number(sValue);
        if (!sValue || !isFinite(iParsed) || iParsed <= 0) {
            return 0;
        }
        return Math.max(1, Math.min(iMax, Math.floor(iParsed)));
    }

    function resolveGrowingPageSize(mState) {
        return resolvePositiveInt((mState || {}).growingPageSize || (mState || {}).searchMaxResults, 9999);
    }

    function resolveMaxResults(mState) {
        return resolveGrowingPageSize(mState);
    }

    function normalizeSearchMaxResultsValue(vValue) {
        var sRaw = String(vValue == null ? "" : vValue).trim();
        var iValue;
        if (!sRaw) {
            return "";
        }
        iValue = resolveGrowingPageSize({ growingPageSize: sRaw });
        return iValue > 0 ? String(iValue) : "";
    }

    function resolveSearchFetchLimit(mState) {
        return resolvePositiveInt((mState || {}).searchFetchLimit || (mState || {}).searchBackendTop, 9999);
    }

    function resolveBackendTop(mState) {
        return resolveSearchFetchLimit(mState);
    }

    function normalizeSearchBackendTopValue(vValue) {
        var sRaw = String(vValue == null ? "" : vValue).trim();
        var iValue;
        if (!sRaw) {
            return "";
        }
        iValue = resolveSearchFetchLimit({ searchFetchLimit: sRaw });
        return iValue > 0 ? String(iValue) : "";
    }

    function resolveExportLimit(mState) {
        var iConfigured = resolvePositiveInt((mState || {}).exportLimit || DEFAULT_EXPORT_LIMIT, DEFAULT_EXPORT_LIMIT);
        return iConfigured || DEFAULT_EXPORT_LIMIT;
    }

    return {
        DEFAULT_EXPORT_LIMIT: DEFAULT_EXPORT_LIMIT,
        normalizeSearchBackendTopValue: normalizeSearchBackendTopValue,
        normalizeSearchMaxResultsValue: normalizeSearchMaxResultsValue,
        resolveBackendTop: resolveBackendTop,
        resolveExportLimit: resolveExportLimit,
        resolveGrowingPageSize: resolveGrowingPageSize,
        resolveMaxResults: resolveMaxResults,
        resolveSearchFetchLimit: resolveSearchFetchLimit
    };
});
