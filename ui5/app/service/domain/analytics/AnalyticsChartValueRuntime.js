sap.ui.define([], function () {
    "use strict";

    var MONTH_KEYS = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"];

    function toNumber(vValue) {
        var nValue = Number(vValue);
        return isFinite(nValue) ? nValue : 0;
    }

    function normalizeChartRows(aRows) {
        var aSource = Array.isArray(aRows) ? aRows : [];
        var nMax = aSource.reduce(function (nAcc, oRow) {
            return Math.max(nAcc, toNumber(oRow && oRow.value));
        }, 0);

        return aSource.map(function (oRow, index) {
            var nValue = toNumber(oRow && oRow.value);
            var sLabel = String((oRow && oRow.label) || "");
            var nHeightRem = nMax > 0 ? Math.max(0.5, (nValue / nMax) * 7.5) : 0.5;

            return {
                label: sLabel,
                labelShort: sLabel.length > 12 ? sLabel.slice(0, 12) + "..." : sLabel,
                value: nValue,
                order: toNumber(oRow && oRow.order) || (index + 1),
                barHeight: nHeightRem.toFixed(2) + "rem"
            };
        });
    }

    function toMonthlyMap(aRows) {
        var mValues = {};
        normalizeChartRows(aRows).forEach(function (oRow, index) {
            var iMonth = toNumber(oRow && oRow.order) || (index + 1);
            mValues[iMonth] = toNumber(oRow && oRow.value);
        });
        return mValues;
    }

    function monthProperty(iMonth) {
        return MONTH_KEYS[Math.max(1, Math.min(12, iMonth)) - 1];
    }

    function hasAnyChartValue(aRows) {
        return (Array.isArray(aRows) ? aRows : []).some(function (oRow) {
            return toNumber(oRow && oRow.value) > 0;
        });
    }

    function hasAnySeriesValue(aRows) {
        return (Array.isArray(aRows) ? aRows : []).some(function (oRow) {
            return toNumber(oRow && oRow.selectedValue) > 0 || toNumber(oRow && oRow.compareValue) > 0;
        });
    }

    return {
        hasAnyChartValue: hasAnyChartValue,
        hasAnySeriesValue: hasAnySeriesValue,
        monthProperty: monthProperty,
        normalizeChartRows: normalizeChartRows,
        toMonthlyMap: toMonthlyMap,
        toNumber: toNumber
    };
});
