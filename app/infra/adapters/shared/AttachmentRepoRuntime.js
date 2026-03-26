sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, ChecklistSnapshotMapper, ODataAdapterUtils, ODataKeyContracts, ODataKeyNormalizer, GatewayContractConstants) {
    "use strict";

    function stripDataUrlPrefix(sValue) {
        return String(sValue || "").replace(/^data:.*?;base64,/i, "").trim();
    }

    function readTransientUploadPayload(oFile) {
        return new Promise(function (resolve, reject) {
            var oReader;
            if (!oFile || typeof FileReader === "undefined" || typeof Blob === "undefined" || !(oFile instanceof Blob)) {
                resolve("");
                return;
            }
            oReader = new FileReader();
            oReader.onload = function (oEvent) {
                resolve(stripDataUrlPrefix(oEvent && oEvent.target && oEvent.target.result));
            };
            oReader.onerror = function () {
                reject(new Error("ATTACHMENT_READ_FAILED"));
            };
            oReader.readAsDataURL(oFile);
        });
    }

    function normalizeRootKey(sRootId) {
        return ODataKeyNormalizer.normalizeBinaryKey(sRootId);
    }

    function mapAttachmentResult(vData) {
        return ODataAdapterUtils.asArray(vData).map(ChecklistSnapshotMapper.mapAttachmentRow);
    }

    function loadAttachments(mArgs) {
        var sRootId = normalizeRootKey(mArgs && mArgs.rootId);
        if (!sRootId) {
            return Promise.resolve({ attachments: [] });
        }
        return GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.ATTACHMENT, {
            "$filter": ODataAdapterUtils.buildEqFilter("PARENT_KEY", sRootId, ODataKeyContracts.TYPES.PARENT_KEY),
            "$select": ODataKeyContracts.SELECTS.ATTACHMENT
        }).then(function (oResult) {
            return { attachments: mapAttachmentResult(oResult) };
        });
    }

    function deleteAttachment(mArgs) {
        var sAttachmentId = String((mArgs && (mArgs.attachmentId || mArgs.attachmentKey)) || "").trim().toUpperCase();
        var sRootId = normalizeRootKey(mArgs && mArgs.rootId);
        if (!sAttachmentId || !sRootId) {
            return Promise.resolve({
                attachmentId: sAttachmentId,
                deleted: false
            });
        }
        return GatewayClient.callFunctionImport(GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES, {
            Payload: {
                root: {
                    pcct_uuid: sRootId,
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

    function uploadAttachment(mArgs) {
        var oAttachment = (mArgs && mArgs.attachment) || {};
        var sRootId = normalizeRootKey((mArgs && mArgs.rootId) || oAttachment.parentKey);
        var sAttachmentId = String(oAttachment.attachmentId || oAttachment.AttachmentKey || "").trim().toUpperCase();
        if (!sRootId || !oAttachment.file) {
            return Promise.resolve(null);
        }
        return readTransientUploadPayload(oAttachment.file).then(function (sBase64) {
            // Canonical persisted attachment state stays on DownloadUrl/DocumentHandle after save.
            return GatewayClient.create("/" + GatewayContractConstants.ENTITY_SETS.ATTACHMENT, {
                AttachmentKey: sAttachmentId || undefined,
                DB_KEY: sRootId,
                PARENT_KEY: normalizeRootKey(oAttachment.parentKey || sRootId),
                FolderKey: String(oAttachment.folderKey || normalizeRootKey(oAttachment.parentKey || sRootId) || sRootId).trim(),
                CategoryKey: String(oAttachment.categoryKey || "GEN").trim() || "GEN",
                Type: String(oAttachment.categoryKey || "GEN").trim() || "GEN",
                FileName: String(oAttachment.fileName || "").trim(),
                Name: String(oAttachment.fileName || "").trim(),
                MimeType: String(oAttachment.mimeType || "application/octet-stream").trim() || "application/octet-stream",
                Description: String(oAttachment.description || "").trim(),
                FileSize: Number(oAttachment.fileSize || 0) || 0,
                FileSizeContent: Number(oAttachment.fileSize || 0) || 0,
                ContentBase64: sBase64
            }).then(function (oResult) {
                return ChecklistSnapshotMapper.mapAttachmentRow(ODataAdapterUtils.unwrap(oResult) || {});
            });
        });
    }

    return {
        normalizeRootKey: normalizeRootKey,
        loadAttachments: loadAttachments,
        deleteAttachment: deleteAttachment,
        uploadAttachment: uploadAttachment
    };
});
