sap.ui.define([], function () {
    "use strict";

    function toLow(v) {
        return String(v || "").toLowerCase();
    }

    function evaluateRateMatch(sFilterKey, nRate) {
        var bFailed = Number.isFinite(nRate) && nRate < 100;
        return sFilterKey === "ALL"
            || (sFilterKey === "TRUE" && bFailed)
            || (sFilterKey === "FALSE" && !bFailed);
    }

    return {
        getSmartFilterConfig: function () {
            return {
                fields: [
                    { key: "filterId", type: "Input", path: "/filterId" },
                    { key: "filterLpc", type: "Select", path: "/filterLpc" },
                    { key: "filterFailedChecks", type: "Segment", path: "/filterFailedChecks" },
                    { key: "filterFailedBarriers", type: "Segment", path: "/filterFailedBarriers" }
                ]
            };
        },

        getSmartTableConfig: function () {
            return {
                columns: ["id", "status", "checks", "barriers", "observer"],
                selectionMode: "SingleSelectMaster"
            };
        },

        filterData: function (aData, mFilters, sSearchMode) {
            var sFilterId = toLow(mFilters.filterId).trim();
            var sFilterLpc = mFilters.filterLpc || "";
            var sChecks = mFilters.filterFailedChecks || "ALL";
            var sBarriers = mFilters.filterFailedBarriers || "ALL";

            return (aData || []).filter(function (oItem) {
                var sId = toLow((((oItem || {}).root || {}).id));
                var sLpc = (((oItem || {}).basic || {}).LPC_KEY || "");
                var nChecks = Number((((oItem || {}).root || {}).successRateChecks));
                var nBarriers = Number((((oItem || {}).root || {}).successRateBarriers));

                var bIdMatch = !sFilterId || sId.includes(sFilterId);
                var bLpcMatch = !sFilterLpc || sLpc === sFilterLpc;
                var bChecksMatch = evaluateRateMatch(sChecks, nChecks);
                var bBarriersMatch = evaluateRateMatch(sBarriers, nBarriers);

                if (sSearchMode === "LOOSE") {
                    var aRules = [
                        { enabled: !!sFilterId, value: bIdMatch },
                        { enabled: !!sFilterLpc, value: bLpcMatch },
                        { enabled: sChecks !== "ALL", value: bChecksMatch },
                        { enabled: sBarriers !== "ALL", value: bBarriersMatch }
                    ].filter(function (oRule) { return oRule.enabled; });

                    return !aRules.length || aRules.some(function (oRule) { return oRule.value; });
                }

                return bIdMatch && bLpcMatch && bChecksMatch && bBarriersMatch;
            });
        }
    };
});
