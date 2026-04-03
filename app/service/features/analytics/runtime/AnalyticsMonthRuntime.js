sap.ui.define([], function () {
    "use strict";

    var MONTH_INDEX = Object.freeze({
        JAN: 0,
        JANUARY: 0,
        ЯНВ: 0,
        ЯНВАРЬ: 0,
        FEB: 1,
        FEBRUARY: 1,
        ФЕВ: 1,
        ФЕВРАЛЬ: 1,
        MAR: 2,
        MARCH: 2,
        МАР: 2,
        МАРТ: 2,
        APR: 3,
        APRIL: 3,
        АПР: 3,
        АПРЕЛЬ: 3,
        MAY: 4,
        МАЙ: 4,
        JUN: 5,
        JUNE: 5,
        ИЮН: 5,
        ИЮНЬ: 5,
        JUL: 6,
        JULY: 6,
        ИЮЛ: 6,
        ИЮЛЬ: 6,
        AUG: 7,
        AUGUST: 7,
        АВГ: 7,
        АВГУСТ: 7,
        SEP: 8,
        SEPT: 8,
        SEPTEMBER: 8,
        СЕН: 8,
        СЕНТЯБРЬ: 8,
        OCT: 9,
        OCTOBER: 9,
        ОКТ: 9,
        ОКТЯБРЬ: 9,
        NOV: 10,
        NOVEMBER: 10,
        НОЯ: 10,
        НОЯБРЬ: 10,
        DEC: 11,
        DECEMBER: 11,
        ДЕК: 11,
        ДЕКАБРЬ: 11
    });

    function resolveMonthIndex(sMonthLabel) {
        return MONTH_INDEX[String(sMonthLabel || "").trim().toUpperCase()];
    }

    return {
        MONTH_INDEX: MONTH_INDEX,
        resolveMonthIndex: resolveMonthIndex
    };
});
