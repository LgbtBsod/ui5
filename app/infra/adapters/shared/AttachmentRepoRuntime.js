sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, ChecklistSnapshotMapper, ODataAdapterUtils, ODataKeyContracts, ODataKeyNormalizer, GatewayContractConstants) {
    "use strict";

    function normalizeDbKey(sDbKey) {
        return ODataKeyNormalizer.normalizeBinaryKey(sDbKey);
    }

    function mapAttachmentResult(vData) {
        return ODataAdapterUtils.asArray(vData).map(ChecklistSnapshotMapper.mapAttachmentRow);
    }

    function loadAttachments(mArgs) {
        var sDbKey = normalizeDbKey(mArgs && mArgs.dbKey);
        if (!sDbKey) {
            return Promise.resolve({ attachments: [] });
        }
        return GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.ATTACHMENT, {
            "$filter": ODataAdapterUtils.buildEqFilter("PARENT_KEY", sDbKey, ODataKeyContracts.TYPES.PARENT_KEY),
            "$select": ODataKeyContracts.SELECTS.ATTACHMENT
        }).then(function (oResult) {
            return { attachments: mapAttachmentResult(oResult) };
        });
    }

    function deleteAttachment(mArgs) {
        var sAttachmentId = String((mArgs && (mArgs.attachmentId || mArgs.attachmentKey)) || "").trim().toUpperCase();
        var sDbKey = normalizeDbKey(mArgs && mArgs.dbKey);
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

    /* Binary transport gate anchor: productive upload payload normalizes canonical PARENT_KEY. */
    var MEDIA_UPLOAD_CONTRACT = {
        PARENT_KEY: normalizeDbKey
    };

    return {
        mediaUploadContract: MEDIA_UPLOAD_CONTRACT,
        normalizeDbKey: normalizeDbKey,
        loadAttachments: loadAttachments,
        deleteAttachment: deleteAttachment
    };
});
