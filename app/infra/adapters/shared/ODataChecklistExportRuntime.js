sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayAdapterSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity"
], function (GatewayAdapterSupport, ChecklistIdentity) {
    "use strict";

    function exportSearchResults(mArgs) {
        var aRootIds = ChecklistIdentity.normalizeChecklistIds((mArgs && (mArgs.rootIds || mArgs.RootKeys)) || []);
        var oPayload = {
            Entity: String((mArgs && mArgs.entity) || "screen").trim() || "screen",
            Limit: Math.max(1, Number((mArgs && mArgs.limit) || 0) || 200000),
            SelectionMode: String((mArgs && mArgs.selectionMode) || (aRootIds.length ? "selected" : "all")).trim() || "all"
        };
        if (aRootIds.length) {
            oPayload.RootKeys = aRootIds;
        }
        if (!aRootIds.length && mArgs && mArgs.searchContract) {
            oPayload.SearchContract = Object.assign({}, mArgs.searchContract);
        }
        return GatewayAdapterSupport.request({
            method: "POST_ENTITY",
            path: "ReportExport",
            body: oPayload
        }).then(function (oResponse) {
            return GatewayAdapterSupport.asArray(oResponse);
        });
    }

    return {
        exportSearchResults: exportSearchResults
    };
});
