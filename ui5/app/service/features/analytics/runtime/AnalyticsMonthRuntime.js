sap.ui.define([], function () {
    "use strict";

    var MONTH_INDEX = Object.freeze({
        JAN: 0,
        JANUARY: 0,
        FEB: 1,
        FEBRUARY: 1,
        MAR: 2,
        MARCH: 2,
        APR: 3,
        APRIL: 3,
        MAY: 4,
        JUN: 5,
        JUNE: 5,
        JUL: 6,
        JULY: 6,
        AUG: 7,
        AUGUST: 7,
        SEP: 8,
        SEPT: 8,
        SEPTEMBER: 8,
        OCT: 9,
        OCTOBER: 9,
        NOV: 10,
        NOVEMBER: 10,
        DEC: 11,
        DECEMBER: 11
    });

    function resolveMonthIndex(sMonthLabel) {
        return MONTH_INDEX[String(sMonthLabel || "").trim().toUpperCase()];
    }

    return {
        MONTH_INDEX: MONTH_INDEX,
        resolveMonthIndex: resolveMonthIndex
    };
});
