sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts"
], function (ODataKeyNormalizer, ODataEntityContracts) {
    "use strict";

    var IDENTITY = ODataEntityContracts.IDENTITY;

    function normalizeRootKey(sRootId) {
        return ODataKeyNormalizer.normalizeBinaryKey(sRootId);
    }

    function resolveServerRootId(oPayload, sFallbackRootId) {
        var oData = oPayload || {};
        return String(
            oData[IDENTITY.ROOT_CANONICAL_FIELDS[0]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[0]] ||
            oData[IDENTITY.ROOT_CANONICAL_FIELDS[1]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[1]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[2]] ||
            (oData.root && (oData.root[IDENTITY.ROOT_ALIAS_FIELDS[2]] || oData.root[IDENTITY.ROOT_CANONICAL_FIELDS[0]] || oData.root[IDENTITY.ROOT_CANONICAL_FIELDS[1]])) ||
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
            oRow.DownloadUrl = String(oRow.DownloadUrl || "").trim();
            oRow.DocumentHandle = String(oRow.DocumentHandle || "").trim();
            return oRow;
        }).filter(function (oRow) {
            return !!(oRow.FileName && (oRow.DownloadUrl || oRow.DocumentHandle));
        });
    }

    function normalizeSavePayload(sRootId, oPayload, aAttachments) {
        var oIn = oPayload || {};
        var aNormalizedAttachments = normalizeAttachmentRows(aAttachments, sRootId);
        return {
            Payload: {
                root: Object.assign({}, oIn.root || {}, {
                    pcct_uuid: normalizeRootKey((oIn.root && (oIn.root.pcct_uuid || oIn.root.RootKey || oIn.root.rootKey)) || sRootId)
                }),
                basic: Object.assign({}, oIn.basic || {}),
                checks: Array.isArray(oIn.checks) ? oIn.checks.slice() : [],
                barriers: Array.isArray(oIn.barriers) ? oIn.barriers.slice() : [],
                participants: Array.isArray(oIn.participants) ? oIn.participants.slice() : [],
                attachments: aNormalizedAttachments.concat(Array.isArray(oIn.attachments) ? oIn.attachments.slice() : [])
            },
            ClientVersion: Number(oIn.client_version || (oIn.root && oIn.root.version_number) || oIn.ClientVersion || 0) || 0,
            SessionGuid: String(oIn.SessionGuid || oIn.session_guid || "").trim() || null
        };
    }

    return {
        normalizeAttachmentRows: normalizeAttachmentRows,
        normalizeRootKey: normalizeRootKey,
        normalizeSavePayload: normalizeSavePayload,
        resolveServerRootId: resolveServerRootId
    };
});
