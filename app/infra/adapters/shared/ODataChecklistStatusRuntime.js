sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, GatewayContractConstants) {
    "use strict";

    function deleteChecklist(mArgs, mDeps) {
        var sRootId = mDeps.normalizeRootKey(mDeps.rootId(mArgs));
        return GatewayClient.callFunctionImport(GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES, {
            Payload: {
                root: {
                    pcct_uuid: sRootId,
                    edit_mode: "D"
                },
                checks: [],
                barriers: [],
                participants: [],
                attachments: [],
                session_guid: String((mArgs && mArgs.sessionGuid) || "").trim() || null,
                client_version: 0
            },
            ClientVersion: 0
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
