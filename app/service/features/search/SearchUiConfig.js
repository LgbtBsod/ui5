sap.ui.define([], function () {
    "use strict";

    var SMART_FILTER_FIELDS = Object.freeze([
        { key: "filterId", type: "Input", path: "/filterId" },
        { key: "filterLpc", type: "Select", path: "/filterLpc" },
        { key: "filterFailedChecks", type: "Segment", path: "/search/checksFailSegment" },
        { key: "filterFailedBarriers", type: "Segment", path: "/search/barriersFailSegment" }
    ]);
    var SMART_TABLE_COLUMNS = Object.freeze(["id", "status", "checks", "barriers", "observer"]);
    var SEARCH_LAYOUT_SEED = Object.freeze({
        smartFilter: Object.freeze({
            fields: SMART_FILTER_FIELDS
        }),
        smartTable: Object.freeze({
            columns: SMART_TABLE_COLUMNS,
            selectionMode: "MultiSelect"
        })
    });

    function cloneArray(aItems) {
        return (aItems || []).map(function (oItem) {
            return Object.assign({}, oItem);
        });
    }

    function getLayoutSeed() {
        return {
            smartFilter: {
                fields: cloneArray(SEARCH_LAYOUT_SEED.smartFilter.fields)
            },
            smartTable: {
                columns: SEARCH_LAYOUT_SEED.smartTable.columns.slice(),
                selectionMode: SEARCH_LAYOUT_SEED.smartTable.selectionMode
            }
        };
    }

    return {
        getLayoutSeed: getLayoutSeed
    };
});
