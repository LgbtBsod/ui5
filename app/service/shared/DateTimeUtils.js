sap.ui.define([
    "sap/ui/core/format/DateFormat"
], function (DateFormat) {
    "use strict";

    var oHumanDateFormatter = DateFormat.getDateInstance({
        pattern: "dd.MM.yyyy"
    });
    var oHumanTimeFormatter = DateFormat.getTimeInstance({
        pattern: "HH:mm"
    });
    var oHumanDateTimeFormatter = DateFormat.getDateTimeInstance({
        pattern: "dd.MM.yyyy, HH:mm"
    });
    var oIsoDateFormatter = DateFormat.getDateInstance({
        pattern: "yyyy-MM-dd",
        UTC: true
    });
    var rODataDate = /^\/Date\((-?\d+)(?:[+-]\d{4})?\)\/$/;

    function toDate(vValue) {
        var sValue;
        var aODataMatch;
        var oDate;

        if (vValue instanceof Date) {
            return Number.isNaN(vValue.getTime()) ? null : vValue;
        }
        if (typeof vValue === "number" && Number.isFinite(vValue)) {
            oDate = new Date(vValue);
            return Number.isNaN(oDate.getTime()) ? null : oDate;
        }

        sValue = String(vValue || "").trim();
        if (!sValue) {
            return null;
        }

        aODataMatch = rODataDate.exec(sValue);
        if (aODataMatch) {
            oDate = new Date(Number(aODataMatch[1]));
            return Number.isNaN(oDate.getTime()) ? null : oDate;
        }

        oDate = oIsoDateFormatter.parse(sValue.slice(0, 10));
        if (oDate) {
            return oDate;
        }

        oDate = oHumanDateFormatter.parse(sValue);
        if (oDate) {
            return oDate;
        }

        oDate = new Date(sValue);
        return Number.isNaN(oDate.getTime()) ? null : oDate;
    }

    function formatHumanDate(vValue) {
        var oDate = toDate(vValue);
        return oDate ? oHumanDateFormatter.format(oDate) : "";
    }

    function formatHumanTime(vValue) {
        var oDate = toDate(vValue) || oHumanTimeFormatter.parse(String(vValue || ""));
        return oDate ? oHumanTimeFormatter.format(oDate) : "";
    }

    function formatHumanDateTime(vValue, oFallbackDate) {
        var oDate = toDate(vValue) || toDate(oFallbackDate);
        return oDate ? oHumanDateTimeFormatter.format(oDate) : "";
    }

    function formatYmdUtc(vValue) {
        var oDate = toDate(vValue);
        return oDate ? oIsoDateFormatter.format(oDate) : "";
    }

    return {
        toDate: toDate,
        formatHumanDate: formatHumanDate,
        formatHumanTime: formatHumanTime,
        formatHumanDateTime: formatHumanDateTime,
        formatYmdUtc: formatYmdUtc
    };
});
