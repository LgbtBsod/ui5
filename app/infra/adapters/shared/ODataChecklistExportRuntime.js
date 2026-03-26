sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ChecklistIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, ODataAdapterUtils, ChecklistIdentity, GatewayContractConstants) {
    "use strict";

    function exportSearchResults(mArgs) {
        var aRootIds = ChecklistIdentity.normalizeChecklistIds((mArgs && (mArgs.rootIds || mArgs.DBKeys || mArgs.RootKeys)) || []);
        var oPayload = {
            Entity: String((mArgs && mArgs.entity) || "screen").trim() || "screen",
            Limit: Math.max(1, Number((mArgs && mArgs.limit) || 0) || 200000),
            SelectionMode: String((mArgs && mArgs.selectionMode) || (aRootIds.length ? "selected" : "all")).trim() || "all"
        };
        if (aRootIds.length) {
            oPayload.DBKeys = aRootIds;
        }
        if (!aRootIds.length && mArgs && mArgs.searchContract) {
            oPayload.SearchContract = Object.assign({}, mArgs.searchContract);
        }
        return GatewayClient.callFunctionImport(GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT, oPayload).then(function (oResponse) {
            return ODataAdapterUtils.asArray(oResponse);
        });
    }

    return {
        exportSearchResults: exportSearchResults
    };
});
