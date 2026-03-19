sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataKeyContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayRequestRuntime, ChecklistSnapshotMapper, ODataAdapterUtils, ODataKeyContracts, GatewayClient) {
    "use strict";

    function normalizeRootKey(sRootId) {
        return String(sRootId || "").replace(/-/g, "").toUpperCase();
    }

    function mapAttachmentResult(vData) {
        return GatewayRequestRuntime.asArray(vData).map(ChecklistSnapshotMapper.mapAttachmentRow);
    }

    function loadAttachments(mArgs) {
        var sRootId = normalizeRootKey(mArgs && mArgs.rootId);
        if (!sRootId) {
            return Promise.resolve({ attachments: [] });
        }
        return GatewayRequestRuntime.get("AttachmentSet", {
            "$filter": ODataAdapterUtils.buildEqFilter("RootKey", sRootId, ODataKeyContracts.TYPES.ROOT_KEY)
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
        return GatewayClient.deletePath(ODataAdapterUtils.buildEntityPath("AttachmentSet", sAttachmentId, {
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
