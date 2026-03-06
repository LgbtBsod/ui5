sap.ui.define([
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap_ui5/util/search/SearchFilterBuilder",
    "sap_ui5/util/search/SearchMaxResults"
], function (Filter, FilterOperator, SearchFilterBuilder, SearchMaxResults) {
    "use strict";

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

    function applyRebindParams(mArgs) {
        var oBindingParams = mArgs.bindingParams || {};
        var mState = mArgs.state || {};
        var fnDataReceived = mArgs.onDataReceived;
        var sSearchMode = String(mState.searchMode || ((mState.search || {}).modeSwitch) || "EXACT").toUpperCase();
        var sChecksSegment = ((mState.search || {}).checksFailSegment) || mState.filterFailedChecks || "ALL";
        var sBarriersSegment = ((mState.search || {}).barriersFailSegment) || mState.filterFailedBarriers || "ALL";
        var oChecksFilter = SearchFilterBuilder.buildFailSegmentFilter(sChecksSegment);
        var oBarriersFilter = SearchFilterBuilder.buildBarrierFailSegmentFilter(sBarriersSegment);
        oBindingParams.filters = sanitizeFilters(oBindingParams.filters || []);
        if (oChecksFilter) { oBindingParams.filters.push(oChecksFilter); }
        if (oBarriersFilter) { oBindingParams.filters.push(oBarriersFilter); }
        if (sSearchMode === "LOOSE" && oBindingParams.filters.length > 1) {
            oBindingParams.filters = [new Filter({ filters: oBindingParams.filters, and: false })];
        }

        var iMax = SearchMaxResults.resolveBackendTop(mState);
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
