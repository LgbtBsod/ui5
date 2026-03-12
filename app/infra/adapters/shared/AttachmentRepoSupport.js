sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayAdapterSupport",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper"
], function (GatewayAdapterSupport, ChecklistSnapshotMapper) {
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
        deleteAttachment: deleteAttachment
    };
});
