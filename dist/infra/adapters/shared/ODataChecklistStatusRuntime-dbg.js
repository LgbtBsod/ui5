sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataKeyContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (ODataAdapterUtils, ODataKeyContracts, GatewayClient) {
    "use strict";

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
        deleteChecklist: deleteChecklist
    };
});
