sap.ui.define([], function () {
    "use strict";

    function resolveMaxResults(mState) {
        var sMax = String((mState || {}).searchMaxResults || "").trim();
        var iParsed = Number(sMax);
        if (!sMax || !isFinite(iParsed) || iParsed <= 0) {
            return 0;
        }
        return Math.max(1, Math.min(9999, Math.floor(iParsed)));
    }

    function normalizeSearchMaxResultsValue(vValue) {
        var sRaw = String(vValue == null ? "" : vValue).trim();
        var iValue;
        if (!sRaw) {
            return "";
        }
        iValue = resolveMaxResults({ searchMaxResults: sRaw });
        return iValue > 0 ? String(iValue) : "";
    }

    function resolveBackendTop(mState) {
        var sTop = String((mState || {}).searchBackendTop || "").trim();
        var iParsed = Number(sTop);
        if (!sTop || !isFinite(iParsed) || iParsed <= 0) {
            return 0;
        }
        return Math.max(1, Math.min(9999, Math.floor(iParsed)));
    }

    function normalizeSearchBackendTopValue(vValue) {
        var sRaw = String(vValue == null ? "" : vValue).trim();
        var iValue;
        if (!sRaw) {
            return "";
        }
        iValue = resolveBackendTop({ searchBackendTop: sRaw });
        return iValue > 0 ? String(iValue) : "";
    }

    return {
        normalizeSearchBackendTopValue: normalizeSearchBackendTopValue,
        normalizeSearchMaxResultsValue: normalizeSearchMaxResultsValue,
        resolveBackendTop: resolveBackendTop,
        resolveMaxResults: resolveMaxResults
    };
});
