sap.ui.define([
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/ui/model/Sorter",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchFilterBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/contracts/SearchMaxResults",
    "PRODUCTION_CONTROL_CHECKLIST/constants/SearchContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (Filter, FilterOperator, Sorter, SearchFilterBuilder, SearchMaxResults, SearchRuntimeConstants, ModelContracts) {
    "use strict";

    var SEARCH_MODE = SearchRuntimeConstants.SEARCH_MODE;
    var SEARCH_SEGMENTS = SearchRuntimeConstants.SEARCH_SEGMENTS;

    function pickFilterValue(vValue) {
        if (typeof vValue === "string") { return vValue; }
        if (Array.isArray(vValue) && vValue.length) { return pickFilterValue(vValue[0]); }
        if (vValue && typeof vValue === "object") {
            if (typeof vValue.value !== "undefined") { return String(vValue.value || ""); }
            if (typeof vValue.key !== "undefined") { return String(vValue.key || ""); }
            if (Array.isArray(vValue.items) && vValue.items.length) { return pickFilterValue(vValue.items[0]); }
            if (Array.isArray(vValue.ranges) && vValue.ranges.length) { return String((vValue.ranges[0] || {}).value1 || ""); }
        }
        return "";
    }

    function sanitizeFilter(oFilter) {
        if (!oFilter) { return null; }
        if (Array.isArray(oFilter.aFilters)) {
            var aChildren = oFilter.aFilters.map(sanitizeFilter).filter(Boolean);
            if (!aChildren.length) { return null; }
            if (aChildren.length === 1) { return aChildren[0]; }
            return new Filter({ filters: aChildren, and: oFilter.bAnd !== false });
        }
        if ((oFilter.oValue1 === "" || oFilter.oValue1 === null || typeof oFilter.oValue1 === "undefined") && oFilter.sOperator !== FilterOperator.BT) { return null; }
        return oFilter;
    }

    function sanitizeFilters(aFilters) {
        return (aFilters || []).map(sanitizeFilter).filter(Boolean);
    }

    function flattenLooseFilters(aFilters) {
        var aSanitized = sanitizeFilters(aFilters);
        if (aSanitized.length !== 1 || !Array.isArray(aSanitized[0].aFilters)) {
            return aSanitized;
        }
        return aSanitized[0].aFilters.map(sanitizeFilter).filter(Boolean);
    }

    function applyRebindParams(mArgs) {
        var oBindingParams = mArgs.bindingParams || {};
        var mState = mArgs.state || {};
        var fnDataReceived = mArgs.onDataReceived;
        var sSearchMode = String(mState.searchMode || SEARCH_MODE.EXACT).toUpperCase();
        var sChecksSegment = ((mState.search || {}).checksFailSegment) || SEARCH_SEGMENTS.ALL;
        var sBarriersSegment = ((mState.search || {}).barriersFailSegment) || SEARCH_SEGMENTS.ALL;
        var oChecksFilter = SearchFilterBuilder.buildFailSegmentFilter(sChecksSegment);
        var oBarriersFilter = SearchFilterBuilder.buildBarrierFailSegmentFilter(sBarriersSegment);
        oBindingParams.filters = sanitizeFilters(oBindingParams.filters || []);
        if (oChecksFilter) { oBindingParams.filters.push(oChecksFilter); }
        if (oBarriersFilter) { oBindingParams.filters.push(oBarriersFilter); }
        if (sSearchMode === SEARCH_MODE.LOOSE && oBindingParams.filters.length > 1) {
            oBindingParams.filters = [new Filter({ filters: oBindingParams.filters, and: false })];
        } else if (sSearchMode === SEARCH_MODE.LOOSE) {
            oBindingParams.filters = flattenLooseFilters(oBindingParams.filters || []);
            if (oBindingParams.filters.length > 1) {
                oBindingParams.filters = [new Filter({ filters: oBindingParams.filters, and: false })];
            }
        }
        applySortAndGroupPolicy(oBindingParams, mState);

        var iMax = SearchMaxResults.resolveSearchFetchLimit(mState);
        oBindingParams.parameters = oBindingParams.parameters || {};
        if (iMax > 0) {
            oBindingParams.parameters.$top = iMax;
            oBindingParams.parameters.top = iMax;
            oBindingParams.parameters.$inlinecount = "none";
            oBindingParams.length = iMax;
        } else {
            if (Object.prototype.hasOwnProperty.call(oBindingParams.parameters, "$top")) { delete oBindingParams.parameters.$top; }
            if (Object.prototype.hasOwnProperty.call(oBindingParams.parameters, "top")) { delete oBindingParams.parameters.top; }
            if (Object.prototype.hasOwnProperty.call(oBindingParams.parameters, "$inlinecount")) { delete oBindingParams.parameters.$inlinecount; }
            if (Object.prototype.hasOwnProperty.call(oBindingParams, "length")) { delete oBindingParams.length; }
        }
        var fnPrevDataReceived = (oBindingParams.events || {}).dataReceived;
        oBindingParams.events = oBindingParams.events || {};
        oBindingParams.events.dataReceived = function (oDataEvent) {
            if (typeof fnPrevDataReceived === "function") { fnPrevDataReceived(oDataEvent); }
            if (typeof fnDataReceived === "function") { fnDataReceived(oDataEvent); }
        };
        return oBindingParams;
    }

    function readSortKey(mState) {
        var sSortKey = String(mState.searchSortKey || "").trim();
        return sSortKey || ModelContracts.TOKENS.DATE_CHECK;
    }

    function readGroupKey(mState) {
        return String(mState.searchGroupKey || "").trim();
    }

    function readBool(vFlag, bDefault) {
        if (typeof vFlag === "boolean") {
            return vFlag;
        }
        return !!bDefault;
    }

    function createGroupFunction(sPath) {
        return function (oContext) {
            var vValue = oContext && oContext.getProperty ? oContext.getProperty(sPath) : "";
            var sText = String(vValue == null ? "-" : vValue).trim() || "-";
            return { key: sText, text: sText };
        };
    }

    function applySortAndGroupPolicy(oBindingParams, mState) {
        var sSortKey = readSortKey(mState || {});
        var sGroupKey = readGroupKey(mState || {});
        var bSortDescending = readBool((mState || {}).searchSortDescending, true);
        var bGroupDescending = readBool((mState || {}).searchGroupDescending, false);
        var aSorters = [];

        if (sGroupKey) {
            aSorters.push(new Sorter(sGroupKey, bGroupDescending, createGroupFunction(sGroupKey)));
        }
        if (sSortKey && (!sGroupKey || sGroupKey !== sSortKey || bGroupDescending !== bSortDescending)) {
            aSorters.push(new Sorter(sSortKey, bSortDescending));
        }
        oBindingParams.sorter = aSorters;
    }

    return {
        pickFilterValue: pickFilterValue,
        sanitizeFilter: sanitizeFilter,
        sanitizeFilters: sanitizeFilters,
        buildFailSegmentFilter: SearchFilterBuilder.buildFailSegmentFilter,
        buildBarrierFailSegmentFilter: SearchFilterBuilder.buildBarrierFailSegmentFilter,
        resolveMaxResults: SearchMaxResults.resolveMaxResults,
        applyRebindParams: applyRebindParams
    };
});
