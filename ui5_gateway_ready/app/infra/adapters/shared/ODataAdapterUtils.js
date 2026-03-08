sap.ui.define([], function () {
    "use strict";

    function unwrap(oData) {
        if (!oData || typeof oData !== "object") {
            return oData;
        }
        if (Object.prototype.hasOwnProperty.call(oData, "d")) {
            return unwrap(oData.d);
        }
        if (Object.prototype.hasOwnProperty.call(oData, "results") && Array.isArray(oData.results)) {
            return oData.results;
        }
        return oData;
    }

    function asArray(oData) {
        var oUnwrapped = unwrap(oData);
        if (Array.isArray(oUnwrapped)) {
            return oUnwrapped;
        }
        if (oUnwrapped && Array.isArray(oUnwrapped.results)) {
            return oUnwrapped.results;
        }
        return [];
    }

    function buildSearchParams(sTerm) {
        return { "$search": String(sTerm || "") };
    }

    function normalizeName(oItem) {
        return String((oItem && (oItem.name || oItem.Name || oItem.locationName || oItem.LocationName || oItem.Text || oItem.location_name)) || "").toLowerCase();
    }

    return {
        unwrap: unwrap,
        asArray: asArray,
        buildSearchParams: buildSearchParams,
        normalizeName: normalizeName
    };
});
