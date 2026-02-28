sap.ui.define([
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator"
], function (Filter, FilterOperator) {
    "use strict";

    function buildFailSegmentFilter(segmentKey) {
        var sKey = String(segmentKey || "ALL").toUpperCase();
        if (sKey === "FAILED") {
            return new Filter("HasFailedChecks", FilterOperator.EQ, true);
        }
        if (sKey === "SUCCESS") {
            return new Filter("HasFailedChecks", FilterOperator.EQ, false);
        }
        return null;
    }

    function buildBarrierFailSegmentFilter(segmentKey) {
        var sKey = String(segmentKey || "ALL").toUpperCase();
        if (sKey === "FAILED") {
            return new Filter("HasFailedBarriers", FilterOperator.EQ, true);
        }
        if (sKey === "SUCCESS") {
            return new Filter("HasFailedBarriers", FilterOperator.EQ, false);
        }
        return null;
    }

    function mergeSmartFilterBarFilters(sfbFilters, extraFilters, mode) {
        var aFilters = Array.isArray(sfbFilters) ? sfbFilters.slice() : [];
        (Array.isArray(extraFilters) ? extraFilters : [extraFilters]).forEach(function (oFilter) {
            if (oFilter) {
                aFilters.push(oFilter);
            }
        });
        if (!aFilters.length) {
            return null;
        }
        return new Filter({
            filters: aFilters,
            and: String(mode || "EXACT").toUpperCase() !== "LOOSE"
        });
    }

    return {
        buildFailSegmentFilter: buildFailSegmentFilter,
        buildBarrierFailSegmentFilter: buildBarrierFailSegmentFilter,
        mergeSmartFilterBarFilters: mergeSmartFilterBarFilters
    };
});
