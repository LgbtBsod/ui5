sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, GatewayContractConstants) {
    "use strict";

    function deleteChecklist(mArgs, mDeps) {
        var sRootId = mDeps.normalizeRootKey(mDeps.rootId(mArgs));
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES, {
            root: {
                pcct_uuid: sRootId,
                edit_mode: "D"
            },
            checks: [],
            barriers: [],
            participants: [],
            attachments: [],
            client_version: 0,
            SessionGuid: String((mArgs && mArgs.sessionGuid) || "").trim() || null
        }).then(function () {
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
