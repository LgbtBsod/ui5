sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataKeyContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayRequestRuntime, ODataAdapterUtils, ODataKeyContracts, GatewayClient) {
    "use strict";

    function setChecklistStatus(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var sStatusCode = mArgs && mArgs.statusCode;
        return GatewayRequestRuntime.postFunction("SetChecklistStatus", {
            RootKey: mDeps.normalizeRootKey(sRootId),
            NewStatus: sStatusCode,
            ClientAggChangedOn: (mArgs && mArgs.clientAggChangedOn) || null
        }).then(function (oResponse) {
            return {
                aggChangedOn: (oResponse && oResponse.AggChangedOn) || "",
                statusCode: sStatusCode
            };
        });
    }

    function deleteChecklist(mArgs, mDeps) {
        var sRootId = mDeps.normalizeRootKey(mDeps.rootId(mArgs));
        return GatewayClient.deletePath(ODataAdapterUtils.buildEntityPath("ChecklistRootSet", sRootId, {
            type: ODataKeyContracts.TYPES.ROOT_KEY
        })).then(function () {
            return {
                deleted: true,
                rootId: sRootId
            };
        });
    }

    return {
        deleteChecklist: deleteChecklist,
        setChecklistStatus: setChecklistStatus
    };
});
