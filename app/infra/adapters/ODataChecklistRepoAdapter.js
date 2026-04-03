sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistSnapshotRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistReadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistStatusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPermissionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistMutationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer"
], function (ChecklistSnapshotMapper, GatewayClient, ODataAdapterUtils, ODataEntityContracts, ODataChecklistPayloadMapper, ODataChecklistSnapshotRuntime, ODataChecklistReadRuntime, ODataChecklistStatusRuntime, ODataChecklistPermissionRuntime, ODataChecklistMutationRuntime, ODataChecklistExportRuntime, DetailRuntimePayload, CreateSentinel, GatewayContractConstants, ODataKeyNormalizer) {
    "use strict";

    function dbKey(mArgs) { return DetailRuntimePayload.dbKey(mArgs); }
    function normalizeDbKey(sDbKey) { return ODataChecklistPayloadMapper.normalizeDbKey(sDbKey); }

    function loadAttachments(mArgs) {
        var sDbKey = ODataKeyNormalizer.normalizeBinaryKey(mArgs && mArgs.dbKey);
        if (!sDbKey) {
            return Promise.resolve({ attachments: [] });
        }
        return GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.ATTACHMENT, {
            "$filter": ODataAdapterUtils.buildEqFilter("PARENT_KEY", sDbKey, ODataEntityContracts.TYPES.PARENT_KEY),
            "$select": ODataEntityContracts.SELECTS.ATTACHMENT
        }).then(function (oResult) {
            return {
                attachments: ODataAdapterUtils.asArray(oResult).map(ChecklistSnapshotMapper.mapAttachmentRow)
            };
        });
    }

    function deleteAttachment(mArgs) {
        var sAttachmentId = String((mArgs && (mArgs.attachmentId || mArgs.attachmentKey)) || "").trim().toUpperCase();
        var sDbKey = ODataKeyNormalizer.normalizeBinaryKey(mArgs && mArgs.dbKey);
        if (!sAttachmentId || !sDbKey) {
            return Promise.resolve({
                attachmentId: sAttachmentId,
                deleted: false
            });
        }
        return GatewayClient.callFunctionImport(GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES, {
            Payload: {
                root: {
                    db_key: sDbKey,
                    edit_mode: "U"
                },
                checks: [],
                barriers: [],
                participants: [],
                attachments: [{
                    attach_uuid: sAttachmentId,
                    AttachmentKey: sAttachmentId,
                    edit_mode: "D"
                }],
                session_guid: String((mArgs && mArgs.sessionGuid) || "").trim() || null,
                client_version: Number((mArgs && mArgs.clientVersion) || 0) || 0
            },
            ClientVersion: Number((mArgs && mArgs.clientVersion) || 0) || 0
        }).then(function () {
            return {
                attachmentId: sAttachmentId,
                deleted: true
            };
        });
    }

    function mutationDeps() {
        return {
            enrichServerSnapshot: function (oServerPayload, sFallbackDbKey) {
                return ODataChecklistReadRuntime.enrichServerSnapshot(oServerPayload, sFallbackDbKey, readDeps());
            },
            normalizeDbKey: normalizeDbKey,
            dbKey: dbKey
        };
    }

    function readDeps() {
        return {
            isCreateId: CreateSentinel.isCreateId,
            resolveServerDbKey: ODataChecklistPayloadMapper.resolveServerDbKey,
            dbKey: dbKey
        };
    }

    function create(mDeps) {
        return {
            resolveDbKey: function (mArgs) {
                return ODataChecklistReadRuntime.resolveDbKey(mArgs, readDeps());
            },
            loadDetailSnapshot: function (mArgs) {
                return ODataChecklistReadRuntime.loadDetailSnapshot(mArgs, readDeps());
            },
            loadDetailRows: function (mArgs) {
                return ODataChecklistReadRuntime.loadDetailRows(mArgs, readDeps());
            },
            saveChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.saveChecklist(mArgs, mutationDeps());
            },
            createChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.createChecklist(mArgs, mutationDeps());
            },
            copyChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.copyChecklist(mArgs, mutationDeps());
            },
            autosaveChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.autosaveChecklist(mArgs, mutationDeps());
            },
            deleteChecklist: function (mArgs) {
                return ODataChecklistStatusRuntime.deleteChecklist(mArgs, mutationDeps());
            },
            exportSearchResults: function (mArgs) {
                return ODataChecklistExportRuntime.exportSearchResults(mArgs);
            },
            checkChecklistPermission: function (mArgs) {
                return ODataChecklistPermissionRuntime.checkChecklistPermission(mArgs, {
                    firstRow: ODataChecklistSnapshotRuntime.firstRow,
                    normalizeDbKey: normalizeDbKey,
                    dbKey: DetailRuntimePayload.dbKey
                });
            },
            loadAttachments: loadAttachments,
            deleteAttachment: deleteAttachment
        };
    }
    return { create: create };
});
