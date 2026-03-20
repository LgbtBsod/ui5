sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataKeyContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (ODataAdapterUtils, ODataKeyContracts, GatewayClient, GatewayContractConstants) {
    "use strict";

    function deleteChecklist(mArgs, mDeps) {
        var sRootId = mDeps.normalizeRootKey(mDeps.rootId(mArgs));
        return GatewayClient.deletePath(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.CHECKLIST_ROOT, sRootId, {
            type: ODataKeyContracts.TYPES.ROOT_KEY
        })).then(function () {
            return {
                deleted: true,
                rootId: sRootId
            };
        });
    }

    return {
        deleteChecklist: deleteChecklist
    };
});
