sap.ui.define([
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/service/backend/GatewayClient"
], function (BackendAdapter, GatewayClient) {
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

    function _escapeValue(sValue) {
        return String(sValue || "").replace(/'/g, "''");
    }

    function _buildFilterExpression(mSafePayload) {
        var aTokens = [];

        if (mSafePayload.filterId) {
            aTokens.push("substringof('" + _escapeValue(mSafePayload.filterId) + "',Id)");
        }

        if (mSafePayload.filterLpc) {
            aTokens.push("Lpc eq '" + _escapeValue(mSafePayload.filterLpc) + "'");
        }

        if (mSafePayload.filterFailedChecks === "FAILED") {
            aTokens.push("HasFailedChecks eq true");
        } else if (mSafePayload.filterFailedChecks === "SUCCESS") {
            aTokens.push("HasFailedChecks eq false");
        }

        if (mSafePayload.filterFailedBarriers === "FAILED") {
            aTokens.push("HasFailedBarriers eq true");
        } else if (mSafePayload.filterFailedBarriers === "SUCCESS") {
            aTokens.push("HasFailedBarriers eq false");
        }

        return aTokens.join(" and ");
    }

    function _loadChecklistSearchSet(mPayload) {
        var mSafePayload = toSafePayload(mPayload);
        var mQuery = {
            "$skip": 0,
            "$top": mSafePayload.maxResults || 100,
            "$inlinecount": "allpages"
        };
        var sFilter = _buildFilterExpression(mSafePayload);
        if (sFilter) {
            mQuery.$filter = sFilter;
        }

        return GatewayClient.readSet("ChecklistSearchSet", mQuery);
    }

    return {

        getCheckLists: function () {
            return _loadChecklistSearchSet({ maxResults: 100 });
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
                return _loadChecklistSearchSet({ maxResults: 100 });
            });
        },

        runSearch: function (mPayload) {
            return _loadChecklistSearchSet(mPayload);
        },

        exportRows: function (sEntity, mPayload, sSearchMode) {
            return BackendAdapter.exportReport(sEntity, {
                filters: toSafePayload(mPayload),
                searchMode: sSearchMode || "EXACT"
            });
        }
    };
});
