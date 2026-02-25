sap.ui.define([
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/service/SmartSearchAdapter"
], function (BackendAdapter, SmartSearchAdapter) {
    "use strict";

    function toSafePayload(mPayload) {
        var sRawMax = String((mPayload || {}).maxResults || "").trim();
        var iMax = sRawMax ? Math.max(1, Math.min(9999, Number(sRawMax) || 0)) : null;

        return {
            filterId: (mPayload && mPayload.filterId) || "",
            filterLpc: (mPayload && mPayload.filterLpc) || "",
            filterFailedChecks: (mPayload && mPayload.filterFailedChecks) || "ALL",
            filterFailedBarriers: (mPayload && mPayload.filterFailedBarriers) || "ALL",
            maxResults: iMax
        };
    }

    return {

        getCheckLists: function () {
            return BackendAdapter.getCheckLists();
        },

        getChecklistById: function (sId) {
            return BackendAdapter.getChecklistRoot(sId).then(function (oChecklist) {
                if (!oChecklist || !oChecklist.root) {
                    return { root: { id: sId } };
                }
                return oChecklist;
            });
        },

        deleteChecklistAndReload: function (sId) {
            return BackendAdapter.deleteCheckList(sId).then(function () {
                return BackendAdapter.getCheckLists();
            });
        },

        runSearch: function (mPayload, sSearchMode, aFallbackCollection) {
            var mSafePayload = toSafePayload(mPayload);
            return BackendAdapter.queryCheckLists({
                idContains: mSafePayload.filterId,
                lpcKey: mSafePayload.filterLpc,
                maxResults: mSafePayload.maxResults
            }).then(function (aPrefiltered) {
                return SmartSearchAdapter.filterData(aPrefiltered, mSafePayload, sSearchMode || "EXACT");
            }).catch(function () {
                return SmartSearchAdapter.filterData(aFallbackCollection || [], mSafePayload, sSearchMode || "EXACT");
            });
        },

        exportRows: function (sEntity, mPayload, sSearchMode) {
            return BackendAdapter.exportReport(sEntity, {
                filters: toSafePayload(mPayload),
                searchMode: sSearchMode || "EXACT"
            });
        }
    };
});
