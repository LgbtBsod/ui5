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
        var sDbKey = mDeps.dbKey(mArgs);
        var oDelta = (mArgs && mArgs.delta) || {};
        var aAttachments = (mArgs && mArgs.attachments) || [];
        var oPayload = ODataChecklistPayloadMapper.normalizeSavePayload(sDbKey, oDelta, aAttachments);
        return buildAggregateEnvelope(
            oPayload,
            mArgs && mArgs.sessionGuid,
            (mArgs && mArgs.clientVersion) || oPayload.client_version
        );
    }

    function enrichSnapshot(oServerPayload, sFallbackDbKey, mDeps) {
        return mDeps.enrichServerSnapshot(oServerPayload, sFallbackDbKey).then(function (oServerSnapshot) {
            return {
                lastChangeSet: {},
                serverResponse: oServerPayload || {},
                serverSnapshot: oServerSnapshot || {}
            };
        });
    }

    function executeAggregateWrite(sFunctionImport, mArgs, mDeps) {
        var sDbKey = mDeps.dbKey(mArgs);
        return GatewayClient.callFunctionImport(sFunctionImport, normalizeAggregatePayload(mArgs, mDeps)).then(function (oServerPayload) {
            return enrichSnapshot(oServerPayload, sDbKey, mDeps);
        });
    }

    function saveChecklist(mArgs, mDeps) {
        return executeAggregateWrite(FUNCTION_IMPORTS.SAVE_CHANGES, mArgs, mDeps);
    }

    function createChecklist(mArgs, mDeps) {
        return executeAggregateWrite(FUNCTION_IMPORTS.CREATE_CHECKLIST, mArgs, mDeps);
    }

    function copyChecklist(mArgs, mDeps) {
        var sDbKey = mDeps.dbKey(mArgs);
        var sSessionGuid = String((mArgs && mArgs.sessionGuid) || "").trim();
        return GatewayClient.callFunctionImport(FUNCTION_IMPORTS.COPY_CHECKLIST, {
            DB_KEY: mDeps.normalizeDbKey(sDbKey),
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
            return mDeps.enrichServerSnapshot(oServerPayload, mDeps.dbKey(mArgs)).then(function (oServerSnapshot) {
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
