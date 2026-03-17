sap.ui.define([], function () {
    "use strict";

    function toTrimmedString(vValue) {
        return String(vValue == null ? "" : vValue).trim();
    }

    function sanitizeYearInput(vValue) {
        return toTrimmedString(vValue).replace(/\D+/g, "").slice(0, 4);
    }

    function normalizeYearString(vValue) {
        var sYear = sanitizeYearInput(vValue);
        var iYear;

        if (!/^\d{4}$/.test(sYear)) {
            return "";
        }
        iYear = Number(sYear);
        return isFinite(iYear) && iYear > 0 ? String(iYear) : "";
    }

    function parseYearOrNull(vValue) {
        var sYear = normalizeYearString(vValue);
        return sYear ? Number(sYear) : null;
    }

    return Object.freeze({
        normalizeYearString: normalizeYearString,
        parseYearOrNull: parseYearOrNull,
        sanitizeYearInput: sanitizeYearInput,
        toTrimmedString: toTrimmedString
    });
});
