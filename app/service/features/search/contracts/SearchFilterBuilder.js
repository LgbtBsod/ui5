sap.ui.define([
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator"
], function (Filter, FilterOperator) {
    "use strict";

    var SEGMENT_KEYS = {
        ALL: "ALL",
        FAILED: "FAILED",
        SUCCESS: "SUCCESS"
    };
    var SEARCH_MODE = Object.freeze({
        EXACT: "EXACT",
        LOOSE: "LOOSE"
    });

    function buildBinarySegmentFilter(segmentKey, sFieldName) {
        var sKey = String(segmentKey || SEGMENT_KEYS.ALL).toUpperCase();
        var mValues = {
            FAILED: true,
            SUCCESS: false
        };
        if (!Object.prototype.hasOwnProperty.call(mValues, sKey)) {
            return null;
        }
        return new Filter(String(sFieldName || ""), FilterOperator.EQ, mValues[sKey]);
    }

    function buildFailSegmentFilter(segmentKey) {
        return buildBinarySegmentFilter(segmentKey, "HasFailedChecks");
    }

    function buildBarrierFailSegmentFilter(segmentKey) {
        return buildBinarySegmentFilter(segmentKey, "HasFailedBarriers");
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
            and: String(mode || SEARCH_MODE.EXACT).toUpperCase() !== SEARCH_MODE.LOOSE
        });
    }

    function buildFilters(options) {
        var mOptions = options || {};
        var sChecksSegment = String(
            mOptions.checksSegment ||
            (mOptions.filterFailedChecks ? "FAILED" : "ALL")
        ).toUpperCase();
        var sBarriersSegment = String(
            mOptions.barriersSegment ||
            (mOptions.filterFailedBarriers ? "FAILED" : "ALL")
        ).toUpperCase();

        return [
            buildFailSegmentFilter(sChecksSegment),
            buildBarrierFailSegmentFilter(sBarriersSegment)
        ].filter(Boolean);
    }

    return {
        buildFailSegmentFilter: buildFailSegmentFilter,
        buildBarrierFailSegmentFilter: buildBarrierFailSegmentFilter,
        mergeSmartFilterBarFilters: mergeSmartFilterBarFilters,
        buildFilters: buildFilters
    };
});
