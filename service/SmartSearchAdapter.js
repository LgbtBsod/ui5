sap.ui.define([], function () {
    "use strict";

    var toLow = (v) => String(v || "").toLowerCase();

    var evaluateRateMatch = (sFilterKey, nRate, vHasFailed) => {
        var bFailed = typeof vHasFailed === "boolean" ? vHasFailed : (Number.isFinite(nRate) && nRate < 100);
        if (sFilterKey === "ALL") {
            return true;
        }

        return (sFilterKey === "TRUE" && bFailed)
            || (sFilterKey === "FALSE" && !bFailed);
    };

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

            return (aData || []).filter((oItem) => {
                var sId = toLow((((oItem || {}).root || {}).id));
                var sLpc = (((oItem || {}).basic || {}).LPC_KEY || "");
                var oRoot = ((oItem || {}).root || {});
                var nChecks = Number(oRoot.successRateChecks);
                var nBarriers = Number(oRoot.successRateBarriers);
                var vChecksFailed = oRoot.hasFailedChecks;
                var vBarriersFailed = oRoot.hasFailedBarriers;

                var bIdMatch = !sFilterId || sId.includes(sFilterId);
                var bLpcMatch = !sFilterLpc || sLpc === sFilterLpc;
                var bChecksMatch = evaluateRateMatch(sChecks, nChecks, vChecksFailed);
                var bBarriersMatch = evaluateRateMatch(sBarriers, nBarriers, vBarriersFailed);

                if (sSearchMode === "LOOSE") {
                    var aRules = [
                        { enabled: !!sFilterId, value: bIdMatch },
                        { enabled: !!sFilterLpc, value: bLpcMatch },
                        { enabled: sChecks !== "ALL", value: bChecksMatch },
                        { enabled: sBarriers !== "ALL", value: bBarriersMatch }
                    ].filter((oRule) => oRule.enabled);

                    return !aRules.length || aRules.some((oRule) => oRule.value);
                }

                return bIdMatch && bLpcMatch && bChecksMatch && bBarriersMatch;
            });
        }
    };
});
