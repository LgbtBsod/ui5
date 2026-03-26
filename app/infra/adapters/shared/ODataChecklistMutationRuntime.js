sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, ODataChecklistPayloadMapper, GatewayContractConstants) {
    "use strict";

    var FUNCTION_IMPORTS = GatewayContractConstants.FUNCTION_IMPORTS;

    function buildAggregateEnvelope(oPayload, sSessionGuid, iClientVersion) {
        var sGuid = String(sSessionGuid || "").trim();
        return {
            Payload: Object.assign({}, oPayload || {}, {
                session_guid: sGuid || null
            }),
            ClientVersion: Number(iClientVersion || (oPayload && oPayload.client_version) || 0) || 0
        };
    }

    function normalizeAggregatePayload(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var oDelta = (mArgs && mArgs.delta) || {};
        var aAttachments = (mArgs && mArgs.attachments) || [];
        var oPayload = ODataChecklistPayloadMapper.normalizeSavePayload(sRootId, oDelta, aAttachments);
        return buildAggregateEnvelope(
            oPayload,
            mArgs && mArgs.sessionGuid,
            (mArgs && mArgs.clientVersion) || oPayload.client_version
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
        return GatewayClient.callFunctionImport(sFunctionImport, normalizeAggregatePayload(mArgs, mDeps)).then(function (oServerPayload) {
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
        return GatewayClient.callFunctionImport(FUNCTION_IMPORTS.COPY_CHECKLIST, {
            DB_KEY: mDeps.normalizeRootKey(sRootId),
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
        return GatewayClient.callFunctionImport(
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
