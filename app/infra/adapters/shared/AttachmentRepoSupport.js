sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayAdapterSupport",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayAdapterSupport, ChecklistSnapshotMapper, GatewayClient) {
    "use strict";

    function normalizeRootKey(sRootId) {
        return String(sRootId || "").replace(/-/g, "").toUpperCase();
    }

    function mapAttachmentResult(vData) {
        return GatewayAdapterSupport.asArray(vData).map(ChecklistSnapshotMapper.mapAttachmentRow);
    }

    function loadAttachments(mArgs) {
        var sRootId = normalizeRootKey(mArgs && mArgs.rootId);
        if (!sRootId) {
            return Promise.resolve({ attachments: [] });
        }
        return GatewayAdapterSupport.get("AttachmentSet", { "$filter": "RootKey eq '" + sRootId + "'" }).then(function (oResult) {
            return { attachments: mapAttachmentResult(oResult) };
        });
    }

    function uploadAttachment(mArgs) {
        var sRootId = normalizeRootKey(mArgs && mArgs.rootId);
        var oFile = mArgs && mArgs.file;
        var oMeta = (mArgs && mArgs.fileMeta) || {};
        var sClientRowId = String((mArgs && mArgs.clientRowId) || "").trim().toUpperCase();
        if (!sRootId || !oFile) {
            return Promise.resolve({ attachments: [] });
        }
        return GatewayClient.createEntity("/AttachmentSet", {
            RootKey: sRootId,
            FolderKey: String(oMeta.folderKey || sRootId).trim() || sRootId,
            CategoryKey: String(oMeta.categoryKey || "GEN").trim() || "GEN",
            FileName: oMeta.fileName || oFile.name || "",
            MimeType: oMeta.mimeType || oFile.type || "application/octet-stream",
            FileSize: Number(oMeta.fileSize || oFile.size || 0) || 0,
            ClientRowId: sClientRowId
        }).then(function (oCreated) {
            var sAttachmentKey = String((oCreated && (oCreated.AttachmentKey || oCreated.Key)) || "").trim().toUpperCase();
            return GatewayClient.putPath("/AttachmentSet(AttachmentKey='" + sAttachmentKey + "')/$value", oFile, {
                contentType: oMeta.mimeType || oFile.type || "application/octet-stream",
                headers: {
                    "Slug": oMeta.fileName || oFile.name || (sAttachmentKey + ".bin"),
                    "X-RootKey": sRootId,
                    "X-CategoryKey": String(oMeta.categoryKey || "GEN").trim() || "GEN"
                }
            }).then(function () {
                var oMapped = ChecklistSnapshotMapper.mapAttachmentRow(Object.assign({}, oCreated, {
                    AttachmentKey: sAttachmentKey,
                    Key: sAttachmentKey,
                    RootKey: sRootId,
                    FolderKey: String(oMeta.folderKey || sRootId).trim() || sRootId,
                    CategoryKey: String(oMeta.categoryKey || "GEN").trim() || "GEN",
                    CategoryText: String(oCreated && oCreated.CategoryText || "").trim(),
                    FileName: oMeta.fileName || oFile.name || "",
                    MimeType: oMeta.mimeType || oFile.type || "application/octet-stream",
                    FileSize: Number(oMeta.fileSize || oFile.size || 0) || 0,
                    ChangedOn: new Date().toISOString(),
                    CreatedOn: String(oCreated && oCreated.CreatedOn || new Date().toISOString())
                }));
                return {
                    attachment: oMapped
                };
            });
        });
    }

    function deleteAttachment(mArgs) {
        var sRootId = normalizeRootKey(mArgs && mArgs.rootId);
        var sAttachmentId = String((mArgs && (mArgs.attachmentId || mArgs.attachmentKey)) || "").trim().toUpperCase();
        if (!sAttachmentId) {
            return Promise.resolve({ deleted: true });
        }
        return GatewayClient.deletePath("/AttachmentSet(AttachmentKey='" + sAttachmentId + "')").then(function () {
            return { deleted: true };
        });
    }

    return {
        normalizeRootKey: normalizeRootKey,
        loadAttachments: loadAttachments,
        uploadAttachment: uploadAttachment,
        deleteAttachment: deleteAttachment
    };
});
