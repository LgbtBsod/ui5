sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ODataChecklistPayloadMapper, GatewayContractConstants) {
    "use strict";

    function withSessionGuid(oRequest, sSessionGuid) {
        var sGuid = String(sSessionGuid || "").trim();
        if (!sGuid) {
            return oRequest;
        }
        return Object.assign({}, oRequest, {
            SessionGuid: sGuid,
            session_guid: sGuid
        });
    }

    function saveChecklist(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var oDelta = (mArgs && mArgs.delta) || {};
        var oRequest = withSessionGuid(
            ODataChecklistPayloadMapper.normalizeSavePayload(sRootId, oDelta, mArgs && mArgs.attachments),
            mArgs && mArgs.sessionGuid
        );
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES, oRequest).then(function (oServerPayload) {
            return mDeps.enrichServerSnapshot(oServerPayload, sRootId).then(function (oServerSnapshot) {
                return {
                    lastChangeSet: {},
                    serverResponse: oServerPayload || {},
                    serverSnapshot: oServerSnapshot || {}
                };
            });
        });
    }

    function createChecklist(mArgs, mDeps) {
        var oCurrent = (mArgs && mArgs.delta) || {};
        var oRequest = withSessionGuid(
            ODataChecklistPayloadMapper.normalizeSavePayload("", oCurrent, mArgs && mArgs.attachments),
            mArgs && mArgs.sessionGuid
        );
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.CREATE_CHECKLIST, oRequest).then(function (oServerPayload) {
            return mDeps.enrichServerSnapshot(oServerPayload, "").then(function (oServerSnapshot) {
                return {
                    lastChangeSet: {},
                    serverResponse: oServerPayload || {},
                    serverSnapshot: oServerSnapshot || {}
                };
            });
        });
    }

    function copyChecklist(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var sSessionGuid = String((mArgs && mArgs.sessionGuid) || "").trim();
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST, {
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
        var sRootId = mDeps.rootId(mArgs);
        var oDelta = (mArgs && mArgs.delta) || {};
        var oRequest = withSessionGuid(
            ODataChecklistPayloadMapper.normalizeSavePayload(sRootId, oDelta),
            mArgs && mArgs.sessionGuid
        );
        return GatewayODataClient.postFunction(GatewayContractConstants.FUNCTION_IMPORTS.AUTO_SAVE, oRequest).then(function (oResponse) {
            return mDeps.enrichServerSnapshot(oResponse, sRootId).then(function (oServerSnapshot) {
                return {
                    autosavedAt: new Date().toISOString(),
                    serverHints: oResponse || {},
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
