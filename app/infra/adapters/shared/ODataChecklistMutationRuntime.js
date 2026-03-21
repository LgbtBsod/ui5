sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ODataChecklistPayloadMapper, GatewayContractConstants) {
    "use strict";

    var FUNCTION_IMPORTS = GatewayContractConstants.FUNCTION_IMPORTS;

    function withSessionGuid(oPayload, sSessionGuid) {
        var sGuid = String(sSessionGuid || "").trim();
        if (!sGuid) {
            return oPayload || {};
        }
        return Object.assign({}, oPayload || {}, {
            SessionGuid: sGuid
        });
    }

    function normalizeAggregatePayload(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var oDelta = (mArgs && mArgs.delta) || {};
        var aAttachments = (mArgs && mArgs.attachments) || [];
        return withSessionGuid(
            ODataChecklistPayloadMapper.normalizeSavePayload(sRootId, oDelta, aAttachments),
            mArgs && mArgs.sessionGuid
        );
    }

    function enrichSnapshot(oServerPayload, sFallbackRootId, mDeps) {
        return mDeps.enrichServerSnapshot(oServerPayload, sFallbackRootId).then(function (oServerSnapshot) {
            return {
                lastChangeSet: {},
                serverResponse: oServerPayload || {},
                serverSnapshot: oServerSnapshot || {}
            };
        });
    }

    function executeAggregateWrite(sFunctionImport, mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        return GatewayODataClient.postFunction(sFunctionImport, normalizeAggregatePayload(mArgs, mDeps)).then(function (oServerPayload) {
            return enrichSnapshot(oServerPayload, sRootId, mDeps);
        });
    }

    function saveChecklist(mArgs, mDeps) {
        return executeAggregateWrite(FUNCTION_IMPORTS.SAVE_CHANGES, mArgs, mDeps);
    }

    function createChecklist(mArgs, mDeps) {
        return executeAggregateWrite(FUNCTION_IMPORTS.CREATE_CHECKLIST, mArgs, mDeps);
    }

    function copyChecklist(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var sSessionGuid = String((mArgs && mArgs.sessionGuid) || "").trim();
        return GatewayODataClient.postFunction(FUNCTION_IMPORTS.COPY_CHECKLIST, {
            RootId: mDeps.normalizeRootKey(sRootId),
            SessionGuid: sSessionGuid
        }).then(function (oServerPayload) {
            return mDeps.enrichServerSnapshot(oServerPayload, "").then(function (oServerSnapshot) {
                return {
                    serverResponse: oServerPayload || {},
                    serverSnapshot: oServerSnapshot || {}
                };
            });
        });
    }

    function autosaveChecklist(mArgs, mDeps) {
        return GatewayODataClient.postFunction(
            FUNCTION_IMPORTS.AUTO_SAVE,
            normalizeAggregatePayload(mArgs, mDeps)
        ).then(function (oServerPayload) {
            return mDeps.enrichServerSnapshot(oServerPayload, mDeps.rootId(mArgs)).then(function (oServerSnapshot) {
                return {
                    autosavedAt: new Date().toISOString(),
                    serverHints: oServerPayload || {},
                    serverSnapshot: oServerSnapshot || {}
                };
            });
        });
    }

    return {
        autosaveChecklist: autosaveChecklist,
        copyChecklist: copyChecklist,
        createChecklist: createChecklist,
        saveChecklist: saveChecklist
    };
});
