sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer"
], function (ODataKeyNormalizer) {
    "use strict";

    function normalizeRootKey(sRootId) {
        return ODataKeyNormalizer.normalizeBinaryKey(sRootId);
    }

    function resolveServerRootId(oPayload, sFallbackRootId) {
        var oData = oPayload || {};
        return String(
            oData.RootKey ||
            oData.rootKey ||
            oData.Key ||
            oData.key ||
            oData.pcct_uuid ||
            (oData.root && (oData.root.pcct_uuid || oData.root.RootKey || oData.root.Key)) ||
            (oData.root && oData.root.id) ||
            sFallbackRootId ||
            ""
        ).trim();
    }

    function normalizeAttachmentRows(aAttachments, sRootId) {
        return (Array.isArray(aAttachments) ? aAttachments : []).map(function (oAttachment) {
            var oRow = Object.assign({}, oAttachment || {});
            oRow.RootKey = normalizeRootKey(oRow.RootKey || sRootId);
            oRow.ParentKey = normalizeRootKey(oRow.ParentKey || oRow.RootKey || sRootId);
            oRow.FolderKey = String(oRow.FolderKey || oRow.ParentKey || oRow.RootKey || sRootId || "").trim();
            oRow.CategoryKey = String(oRow.CategoryKey || oRow.Type || "GEN").trim() || "GEN";
            oRow.Type = String(oRow.Type || oRow.CategoryKey || "GEN").trim() || "GEN";
            oRow.FileName = String(oRow.FileName || oRow.Name || "").trim();
            oRow.Name = String(oRow.Name || oRow.FileName || "").trim();
            oRow.MimeType = String(oRow.MimeType || "application/octet-stream").trim() || "application/octet-stream";
            oRow.FileSize = Number(oRow.FileSize || oRow.FileSizeContent || 0) || 0;
            oRow.FileSizeContent = Number(oRow.FileSizeContent || oRow.FileSize || 0) || 0;
            oRow.Description = String(oRow.Description || "").trim();
            oRow.Value = String(oRow.Value || "").trim();
            return oRow;
        }).filter(function (oRow) {
            return !!(oRow.FileName && oRow.Value);
        });
    }

    function normalizeSavePayload(sRootId, oPayload, aAttachments) {
        var oIn = oPayload || {};
        var aNormalizedAttachments = normalizeAttachmentRows(aAttachments, sRootId);
        if (
            Object.prototype.hasOwnProperty.call(oIn, "root") ||
            Object.prototype.hasOwnProperty.call(oIn, "checks") ||
            Object.prototype.hasOwnProperty.call(oIn, "barriers") ||
            Object.prototype.hasOwnProperty.call(oIn, "participants") ||
            Object.prototype.hasOwnProperty.call(oIn, "attachments")
        ) {
            var aUnifiedAttachments = Array.isArray(oIn.attachments) ? oIn.attachments.slice() : [];
            return Object.assign({}, oIn, {
                root: Object.assign({}, oIn.root || {}, {
                    pcct_uuid: normalizeRootKey((oIn.root && oIn.root.pcct_uuid) || sRootId)
                }),
                participants: Array.isArray(oIn.participants) ? oIn.participants.slice() : [],
                attachments: aUnifiedAttachments.concat(aNormalizedAttachments),
                client_version: Number(oIn.client_version || ((oIn.root || {}).version_number) || 0) || 0,
                SessionGuid: oIn.SessionGuid || oIn.session_guid || null
            });
        }
        return {
            RootKey: normalizeRootKey(sRootId),
            ClientAggChangedOn: (oIn.meta && oIn.meta.aggChangedOn) || null,
            FullPayload: {
                root: oIn.root || {},
                basic: oIn.basic || {},
                checks: oIn.checks || [],
                barriers: oIn.barriers || [],
                participants: oIn.participants || [],
                attachments: aNormalizedAttachments
            }
        };
    }

    return {
        normalizeAttachmentRows: normalizeAttachmentRows,
        normalizeRootKey: normalizeRootKey,
        normalizeSavePayload: normalizeSavePayload,
        resolveServerRootId: resolveServerRootId
    };
});
