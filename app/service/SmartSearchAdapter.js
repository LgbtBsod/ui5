sap.ui.define([], function () {
    "use strict";

    var toLow = function (v) { return String(v || "").toLowerCase(); };

    function readFlag(oRoot, sCamel, sSnake) {
        if (!oRoot || typeof oRoot !== "object") {
            return undefined;
        }
        if (typeof oRoot[sCamel] === "boolean") {
            return oRoot[sCamel];
        }
        if (typeof oRoot[sSnake] === "boolean") {
            return oRoot[sSnake];
        }
        return undefined;
    }

    function evaluateRateMatch(sFilterKey, nRate, vHasFailed) {
        var bFailed = typeof vHasFailed === "boolean" ? vHasFailed : (Number.isFinite(nRate) && nRate < 100);
        if (sFilterKey === "ALL") {
            return true;
        }
        return (sFilterKey === "TRUE" && bFailed) || (sFilterKey === "FALSE" && !bFailed);
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

        // Runtime search is server-side via OData; this helper is retained for legacy flows/tests only.
        filterData: function (aData, mFilters, sSearchMode) {
            var sFilterId = toLow((mFilters && mFilters.filterId) || "").trim();
            var sFilterLpc = (mFilters && mFilters.filterLpc) || "";
            var sChecks = (mFilters && mFilters.filterFailedChecks) || "ALL";
            var sBarriers = (mFilters && mFilters.filterFailedBarriers) || "ALL";

            return (aData || []).filter(function (oItem) {
                var oRoot = ((oItem || {}).root || {});
                var oBasic = ((oItem || {}).basic || {});
                var sId = toLow(oRoot.id);
                var sChecklistId = toLow(oBasic.checklist_id || oRoot.checklist_id || oRoot.CHECKLIST_ID || "");
                var sLpc = (oBasic.LPC_KEY || oRoot.lpc || oRoot.LPC || "");
                var nChecks = Number(oRoot.successRateChecks || oRoot.success_rate_checks);
                var nBarriers = Number(oRoot.successRateBarriers || oRoot.success_rate_barriers);
                var vChecksFailed = readFlag(oRoot, "hasFailedChecks", "has_failed_checks");
                var vBarriersFailed = readFlag(oRoot, "hasFailedBarriers", "has_failed_barriers");

                var bIdMatch = !sFilterId || sId.indexOf(sFilterId) >= 0 || sChecklistId.indexOf(sFilterId) >= 0;
                var bLpcMatch = !sFilterLpc || sLpc === sFilterLpc;
                var bChecksMatch = evaluateRateMatch(sChecks, nChecks, vChecksFailed);
                var bBarriersMatch = evaluateRateMatch(sBarriers, nBarriers, vBarriersFailed);

                if (String(sSearchMode || "EXACT").toUpperCase() === "LOOSE") {
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
