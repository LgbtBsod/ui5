sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper"
], function (GatewayRequestRuntime, ODataChecklistPayloadMapper) {
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
        return GatewayRequestRuntime.request({
            method: "POST_ENTITY",
            path: "SaveChanges",
            body: oRequest
        }).then(function (oServerPayload) {
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
        var oRequest = ODataChecklistPayloadMapper.normalizeSavePayload("", oCurrent, mArgs && mArgs.attachments);
        return GatewayRequestRuntime.request({
            method: "POST_ENTITY",
            path: "CreateChecklist",
            body: oRequest
        }).then(function (oServerPayload) {
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
        return GatewayRequestRuntime.postFunction("CopyChecklist", {
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
        return GatewayRequestRuntime.request({
            method: "POST_ENTITY",
            path: "AutoSave",
            body: oRequest
        }).then(function (oResponse) {
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
