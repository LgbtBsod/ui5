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

    function normalizeRootKey(sRootId) {
        return ODataKeyNormalizer.normalizeBinaryKey(sRootId);
    }

    function resolveServerRootId(oPayload, sFallbackRootId) {
        var oData = oPayload || {};
        return normalizeRootKey(
            oData[IDENTITY.ROOT_CANONICAL_FIELDS[0]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[0]] ||
            oData[IDENTITY.ROOT_CANONICAL_FIELDS[1]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[1]] ||
            oData[IDENTITY.ROOT_ALIAS_FIELDS[2]] ||
            (oData.root && (oData.root[IDENTITY.ROOT_ALIAS_FIELDS[2]] || oData.root[IDENTITY.ROOT_CANONICAL_FIELDS[0]] || oData.root[IDENTITY.ROOT_CANONICAL_FIELDS[1]])) ||
            (oData.root && oData.root.id) ||
            sFallbackRootId ||
            ""
        );
    }

    function normalizeAttachmentRows(aAttachments, sRootId) {
        return (Array.isArray(aAttachments) ? aAttachments : []).map(function (oAttachment) {
            var oRow = Object.assign({}, oAttachment || {});
            var sParentKey = normalizeRootKey(oRow.PARENT_KEY || sRootId);
            oRow.DB_KEY = normalizeRootKey(oRow.DB_KEY || "");
            oRow.PARENT_KEY = sParentKey;
            oRow.FolderKey = String(oRow.FolderKey || sParentKey || sRootId || "").trim();
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

    function normalizeSavePayload(sRootId, oPayload, aAttachments) {
        var oIn = oPayload || {};
        var aNormalizedAttachments = normalizeAttachmentRows(aAttachments, sRootId);
        return {
            Payload: {
                root: Object.assign({}, oIn.root || {}, {
                    DB_KEY: normalizeRootKey((oIn.root && oIn.root.DB_KEY) || sRootId)
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
        applyBasicFieldAlias: applyBasicFieldAlias,
        mapBasicFieldName: mapBasicFieldName,
        normalizeAttachmentRows: normalizeAttachmentRows,
        normalizeRootKey: normalizeRootKey,
        normalizeSavePayload: normalizeSavePayload,
        pickBasicFieldValue: pickBasicFieldValue,
        resolveServerRootId: resolveServerRootId
    };
});
