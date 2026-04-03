sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts"
], function (ODataKeyNormalizer, ODataEntityContracts) {
    "use strict";

    var IDENTITY = ODataEntityContracts.IDENTITY;
    var BASIC_FIELD_ALIASES = Object.freeze({
        BARRIERS_NUMBER: "BarriersNumber",
        CHECKS_NUMBER: "ChecksNumber",
        LPC_KEY: "Lpc",
        PROF_KEY: "Profession"
    });
    var BASIC_FIELD_SYNONYMS = Object.freeze({
        BarriersNumber: Object.freeze(["BarriersNumber", "BARRIERS_NUMBER"]),
        ChecksNumber: Object.freeze(["ChecksNumber", "CHECKS_NUMBER"]),
        Lpc: Object.freeze(["Lpc", "LPC_KEY"]),
        Profession: Object.freeze(["Profession", "PROF_KEY"])
    });

    function normalizeDbKey(sDbKey) {
        return ODataKeyNormalizer.normalizeBinaryKey(sDbKey);
    }

    function resolveServerDbKey(oPayload, sFallbackDbKey) {
        var oData = oPayload || {};
        var oRoot = oData.root || {};
        return normalizeDbKey(
            oData[IDENTITY.ROOT_CANONICAL_FIELDS[0]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[0]] ||
            oData[IDENTITY.ROOT_CANONICAL_FIELDS[1]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[1]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[2]] ||
            oRoot[IDENTITY.ROOT_ALIAS_FIELDS[2]] ||
            oRoot[IDENTITY.ROOT_CANONICAL_FIELDS[0]] ||
            oRoot[IDENTITY.ROOT_CANONICAL_FIELDS[1]] ||
            oRoot.id ||
            sFallbackDbKey ||
            ""
        );
    }

    function normalizeAttachmentRows(aAttachments, sDbKey) {
        return (Array.isArray(aAttachments) ? aAttachments : []).map(function (oAttachment) {
            var oRow = Object.assign({}, oAttachment || {});
            var sParentKey = normalizeDbKey(oRow.PARENT_KEY || sDbKey);
            oRow.DB_KEY = normalizeDbKey(oRow.DB_KEY || "");
            oRow.PARENT_KEY = sParentKey;
            oRow.FolderKey = String(oRow.FolderKey || sParentKey || sDbKey || "").trim();
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

    function normalizeMutationAttachmentRows(aAttachments, sDbKey) {
        return (Array.isArray(aAttachments) ? aAttachments : []).map(function (oAttachment) {
            var oRow = Object.assign({}, oAttachment || {});
            var sParentKey = normalizeDbKey(oRow.PARENT_KEY || oRow.parent_key || sDbKey);
            var sAttachmentKey = String(oRow.AttachmentKey || oRow.attach_uuid || oRow.client_row_id || oRow.Key || "").trim();
            return {
                attach_uuid: String(oRow.attach_uuid || (String(oRow.edit_mode || oRow.EditMode || "").toUpperCase() === "C" ? "" : sAttachmentKey)).trim(),
                client_row_id: String(oRow.client_row_id || sAttachmentKey || "").trim(),
                edit_mode: String(oRow.edit_mode || oRow.EditMode || "U").trim().toUpperCase() || "U",
                root_key: normalizeDbKey(oRow.root_key || oRow.DB_KEY || sDbKey),
                parent_key: sParentKey,
                folder_key: String(oRow.folder_key || oRow.FolderKey || sParentKey || sDbKey || "").trim(),
                category_key: String(oRow.category_key || oRow.CategoryKey || oRow.Type || "GEN").trim() || "GEN",
                file_name: String(oRow.file_name || oRow.FileName || oRow.Name || "").trim(),
                mime_type: String(oRow.mime_type || oRow.MimeType || "application/octet-stream").trim() || "application/octet-stream",
                description: String(oRow.description || oRow.Description || "").trim(),
                file_size: Number(oRow.file_size || oRow.FileSize || oRow.FileSizeContent || 0) || 0
            };
        }).filter(function (oRow) {
            return !!(oRow.edit_mode && (oRow.file_name || oRow.attach_uuid || oRow.client_row_id));
        });
    }

    function mapBasicFieldName(sFieldName) {
        var sField = String(sFieldName || "").trim();
        return BASIC_FIELD_ALIASES[sField] || sField;
    }

    function applyBasicFieldAlias(oBasic, sFieldName, vValue) {
        var oOut = Object.assign({}, oBasic || {});
        var sMappedField = mapBasicFieldName(sFieldName);
        if (!sMappedField) {
            return oOut;
        }
        oOut[sMappedField] = vValue;
        return oOut;
    }

    function pickBasicFieldValue(oBasic, sFieldName) {
        var sMappedField = mapBasicFieldName(sFieldName);
        var aCandidates = BASIC_FIELD_SYNONYMS[sMappedField] || [sMappedField];
        var i;
        var sCandidate;
        for (i = 0; i < aCandidates.length; i += 1) {
            sCandidate = aCandidates[i];
            if (oBasic && Object.prototype.hasOwnProperty.call(oBasic, sCandidate)) {
                return oBasic[sCandidate];
            }
        }
        return undefined;
    }

    function normalizeSavePayload(sDbKey, oPayload, aAttachments) {
        var oIn = oPayload || {};
        var aCanonicalAttachments = Array.isArray(oIn.attachments) && oIn.attachments.length
            ? oIn.attachments
            : aAttachments;
        var aNormalizedAttachments = normalizeMutationAttachmentRows(aCanonicalAttachments, sDbKey);
        return {
            Payload: {
                root: Object.assign({}, oIn.root || {}, {
                    DB_KEY: normalizeDbKey((oIn.root && (oIn.root.DB_KEY || oIn.root.db_key)) || sDbKey),
                    db_key: normalizeDbKey((oIn.root && (oIn.root.db_key || oIn.root.DB_KEY)) || sDbKey)
                }),
                basic: Object.assign({}, oIn.basic || {}),
                checks: Array.isArray(oIn.checks) ? oIn.checks.slice() : [],
                barriers: Array.isArray(oIn.barriers) ? oIn.barriers.slice() : [],
                participants: Array.isArray(oIn.participants) ? oIn.participants.slice() : [],
                attachments: aNormalizedAttachments
            },
            ClientVersion: Number(oIn.client_version || (oIn.root && oIn.root.version_number) || oIn.ClientVersion || 0) || 0,
            SessionGuid: String(oIn.SessionGuid || oIn.session_guid || "").trim() || null
        };
    }

    return {
        applyBasicFieldAlias: applyBasicFieldAlias,
        mapBasicFieldName: mapBasicFieldName,
        normalizeAttachmentRows: normalizeAttachmentRows,
        normalizeMutationAttachmentRows: normalizeMutationAttachmentRows,
        normalizeDbKey: normalizeDbKey,
        normalizeSavePayload: normalizeSavePayload,
        pickBasicFieldValue: pickBasicFieldValue,
        resolveServerDbKey: resolveServerDbKey
    };
});
