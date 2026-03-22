sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ChecklistSnapshotMapper, ODataAdapterUtils, ODataKeyContracts, GatewayClient, ODataKeyNormalizer, GatewayContractConstants) {
    "use strict";

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
        return GatewayODataClient.get(GatewayContractConstants.ENTITY_SETS.ATTACHMENT, {
            "$filter": ODataAdapterUtils.buildEqFilter("RootKey", sRootId, ODataKeyContracts.TYPES.ROOT_KEY),
            "$select": "AttachmentKey,Key,RootKey,FolderKey,CategoryKey,CategoryText,Type,FileName,Name,MimeType,Description,FileSize,ScanStatus,ScannedOn,CreatedOn,ChangedOn"
        }).then(function (oResult) {
            return { attachments: mapAttachmentResult(oResult) };
        });
    }

    function deleteAttachment(mArgs) {
        var sRootId = normalizeRootKey(mArgs && mArgs.rootId);
        var sAttachmentId = String((mArgs && (mArgs.attachmentId || mArgs.attachmentKey)) || "").trim().toUpperCase();
        if (!sAttachmentId) {
            return Promise.resolve({ deleted: true });
        }
        return GatewayClient.deletePath(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.ATTACHMENT, sAttachmentId, {
            name: "AttachmentKey",
            type: ODataKeyContracts.TYPES.ATTACHMENT_KEY
        })).then(function () {
            return { deleted: true };
        });
    }

    return {
        normalizeRootKey: normalizeRootKey,
        loadAttachments: loadAttachments,
        deleteAttachment: deleteAttachment
    };
});
