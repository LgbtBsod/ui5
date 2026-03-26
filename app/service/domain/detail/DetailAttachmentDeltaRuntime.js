sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (DeltaContracts, JsRuntime) {
    "use strict";

    function readAttachmentKey(oAttachment) {
        return String((oAttachment && (oAttachment.client_row_id || oAttachment.AttachmentKey || oAttachment.attach_uuid || oAttachment.Key)) || "").trim();
    }

    function hasStagedFile(oAttachment) {
        var sUploadState = String((oAttachment && oAttachment.uploadState) || "").trim().toUpperCase();
        return !!(
            oAttachment && (
                (oAttachment.staged && oAttachment._file) ||
                sUploadState === "PENDINGUPLOAD" ||
                sUploadState === "PENDING_UPLOAD"
            )
        );
    }

    function serializeStagedAttachments(aAttachments, sRootId) {
        return Promise.resolve([]);
    }

    function mergeDeltaAttachments(oDelta, aAttachmentRows) {
        var oPayload = Object.assign({}, oDelta || {});
        var mByKey = {};
        var aAnonymous = [];

        (Array.isArray(oPayload.attachments) ? oPayload.attachments : []).forEach(function (oRow) {
            var sKey = readAttachmentKey(oRow);
            if (sKey) {
                mByKey[sKey] = Object.assign({}, oRow);
                return;
            }
            aAnonymous.push(Object.assign({}, oRow));
        });

        (Array.isArray(aAttachmentRows) ? aAttachmentRows : []).forEach(function (oRow) {
            var sKey = readAttachmentKey(oRow);
            if (sKey) {
                mByKey[sKey] = Object.assign({}, mByKey[sKey] || {}, oRow);
                return;
            }
            aAnonymous.push(Object.assign({}, oRow));
        });

        oPayload.attachments = Object.keys(mByKey).map(function (sKey) {
            return mByKey[sKey];
        }).concat(aAnonymous);
        return oPayload;
    }

    function refreshAttachments(oRepo, sRootId, aCurrentAttachments, bForceReload) {
        if (!sRootId || !oRepo || typeof oRepo.loadAttachments !== JsRuntime.TYPEOF.FUNCTION || !bForceReload) {
            return Promise.resolve(Array.isArray(aCurrentAttachments) ? aCurrentAttachments : []);
        }
            return oRepo.loadAttachments({ dbKey: sRootId }).then(function (oLoaded) {
                return (oLoaded && oLoaded.attachments) || [];
            }).catch(function () {
            return Array.isArray(aCurrentAttachments) ? aCurrentAttachments : [];
        });
    }

    function revokeLocalUrl(oAttachment) {
        var sUrl = oAttachment && oAttachment.localObjectUrl;
        if (sUrl && typeof window !== "undefined" && window.URL && typeof window.URL.revokeObjectURL === JsRuntime.TYPEOF.FUNCTION) {
            window.URL.revokeObjectURL(sUrl);
        }
    }

    function cleanupStagedAttachmentUrls(aAttachments) {
        (Array.isArray(aAttachments) ? aAttachments : []).forEach(revokeLocalUrl);
    }

    function stripStagedAttachmentInternals(aAttachments) {
        return (Array.isArray(aAttachments) ? aAttachments : []).map(function (oAttachment) {
            var oClean = Object.assign({}, oAttachment || {});
            delete oClean._file;
            delete oClean.staged;
            delete oClean.uploadState;
            delete oClean.localObjectUrl;
            return oClean;
        });
    }

    function listPendingStagedAttachments(aAttachments, sRootId) {
        return (Array.isArray(aAttachments) ? aAttachments : []).filter(hasStagedFile).map(function (oAttachment) {
            return {
                attachmentId: readAttachmentKey(oAttachment),
                dbKey: String((oAttachment && (oAttachment.DB_KEY || oAttachment.dbKey)) || "").trim(),
                parentKey: String((oAttachment && (oAttachment.PARENT_KEY || oAttachment.parentKey || oAttachment.ParentKey)) || sRootId || "").trim(),
                folderKey: String((oAttachment && (oAttachment.FolderKey || oAttachment.folderKey)) || sRootId || "").trim(),
                categoryKey: String((oAttachment && (oAttachment.CategoryKey || oAttachment.categoryKey || oAttachment.Type || oAttachment.type)) || "GEN").trim() || "GEN",
                fileName: String((oAttachment && (oAttachment.FileName || oAttachment.fileName || oAttachment.Name || oAttachment.name)) || "").trim(),
                mimeType: String((oAttachment && (oAttachment.MimeType || oAttachment.mimeType)) || "application/octet-stream").trim() || "application/octet-stream",
                description: String((oAttachment && (oAttachment.Description || oAttachment.description || oAttachment.Desc || oAttachment.desc)) || "").trim(),
                fileSize: Number((oAttachment && (oAttachment.FileSize || oAttachment.fileSize || oAttachment.FileSizeContent || oAttachment.fileSizeContent)) || 0) || 0,
                file: oAttachment && oAttachment._file
            };
        });
    }

    function hasPendingStagedAttachments(aAttachments) {
        return (Array.isArray(aAttachments) ? aAttachments : []).some(hasStagedFile);
    }

    return {
        cleanupStagedAttachmentUrls: cleanupStagedAttachmentUrls,
        hasPendingStagedAttachments: hasPendingStagedAttachments,
        listPendingStagedAttachments: listPendingStagedAttachments,
        mergeDeltaAttachments: mergeDeltaAttachments,
        refreshAttachments: refreshAttachments,
        serializeStagedAttachments: serializeStagedAttachments,
        stripStagedAttachmentInternals: stripStagedAttachmentInternals
    };
});
