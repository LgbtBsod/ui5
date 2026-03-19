sap.ui.define([
    "sap/ui/model/odata/ODataUtils"
], function (ODataUtils) {
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

    function formatLiteral(vValue, sType) {
        if (vValue === undefined || vValue === null) {
            return "null";
        }
        if (sType && ODataUtils && typeof ODataUtils.formatValue === "function") {
            return ODataUtils.formatValue(vValue, sType);
        }
        if (typeof vValue === "string") {
            return "'" + String(vValue).replace(/'/g, "''") + "'";
        }
        return String(vValue);
    }

    function buildKeyPredicate(vKey, vSpec) {
        var oSpec = typeof vSpec === "string" ? { name: vSpec } : (vSpec || {});
        if (Array.isArray(vKey)) {
            return vKey.map(function (oPart) {
                return buildKeyPredicate(oPart.value, oPart);
            }).join(",");
        }
        if (vKey && typeof vKey === "object" && !Array.isArray(vKey)) {
            return Object.keys(vKey).map(function (sName) {
                var oPart = (oSpec && oSpec[sName]) || {};
                return sName + "=" + formatLiteral(vKey[sName], oPart.type);
            }).join(",");
        }
        if (oSpec.name) {
            return oSpec.name + "=" + formatLiteral(vKey, oSpec.type);
        }
        return formatLiteral(vKey, oSpec.type);
    }

    function buildEntityPath(sEntitySet, vKey, vSpec) {
        return "/" + String(sEntitySet || "").replace(/^\/+/, "") + "(" + buildKeyPredicate(vKey, vSpec) + ")";
    }

    function buildEqFilter(sProperty, vValue, sType) {
        return String(sProperty || "").trim() + " eq " + formatLiteral(vValue, sType);
    }

    return {
        unwrap: unwrap,
        asArray: asArray,
        buildSearchParams: buildSearchParams,
        buildEntityPath: buildEntityPath,
        buildEqFilter: buildEqFilter,
        buildKeyPredicate: buildKeyPredicate,
        formatLiteral: formatLiteral,
        normalizeName: normalizeName
    };
});
