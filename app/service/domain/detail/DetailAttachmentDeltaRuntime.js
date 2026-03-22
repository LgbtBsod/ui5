sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/AttachmentValueCodec",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/delta/DeltaContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants"
], function (AttachmentValueCodec, DeltaContracts, JsRuntime) {
    "use strict";

    function readAttachmentKey(oAttachment) {
        return String((oAttachment && (oAttachment.client_row_id || oAttachment.AttachmentKey || oAttachment.attach_uuid || oAttachment.Key)) || "").trim();
    }

    function hasStagedFile(oAttachment) {
        var sUploadState = String((oAttachment && oAttachment.uploadState) || "").trim().toUpperCase();
        return !!(
            oAttachment && (
                (oAttachment.staged && (oAttachment._file || oAttachment._fileBase64)) ||
                sUploadState === "PENDINGUPLOAD" ||
                sUploadState === "PENDING_UPLOAD"
            )
        );
    }

    function buildStagedAttachmentPayload(oAttachment, sRootId) {
        return AttachmentValueCodec.fileToBase64(oAttachment && oAttachment._file).then(function (sValue) {
            return {
                client_row_id: readAttachmentKey(oAttachment),
                root_key: String((oAttachment && (oAttachment.RootKey || oAttachment.rootKey)) || sRootId || "").trim(),
                parent_key: String((oAttachment && (oAttachment.ParentKey || oAttachment.parentKey || oAttachment.RootKey || oAttachment.rootKey)) || sRootId || "").trim(),
                folder_key: String((oAttachment && (oAttachment.FolderKey || oAttachment.folderKey || oAttachment.ParentKey || oAttachment.parentKey)) || sRootId || "").trim(),
                category_key: String((oAttachment && (oAttachment.CategoryKey || oAttachment.categoryKey || oAttachment.Type || oAttachment.type)) || "GEN").trim() || "GEN",
                file_name: String((oAttachment && (oAttachment.FileName || oAttachment.fileName || oAttachment.Name || oAttachment.name)) || "").trim(),
                mime_type: String((oAttachment && (oAttachment.MimeType || oAttachment.mimeType)) || "application/octet-stream").trim() || "application/octet-stream",
                description: String((oAttachment && (oAttachment.Description || oAttachment.description || oAttachment.Desc || oAttachment.desc)) || "").trim(),
                file_size: Number((oAttachment && (oAttachment.FileSize || oAttachment.fileSize || oAttachment.FileSizeContent || oAttachment.fileSizeContent)) || 0) || 0,
                value: sValue,
                edit_mode: DeltaContracts.EDIT_MODE.CREATE
            };
        });
    }

    function serializeStagedAttachments(aAttachments, sRootId) {
        var aPending = (Array.isArray(aAttachments) ? aAttachments : []).filter(hasStagedFile);
        if (!aPending.length) {
            return Promise.resolve([]);
        }
        return Promise.all(aPending.map(function (oAttachment) {
            return buildStagedAttachmentPayload(oAttachment, sRootId);
        }));
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
        return oRepo.loadAttachments({ rootId: sRootId }).then(function (oLoaded) {
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
            delete oClean.localObjectUrl;
            delete oClean.staged;
            return oClean;
        });
    }

    function hasPendingStagedAttachments(aAttachments) {
        return (Array.isArray(aAttachments) ? aAttachments : []).some(hasStagedFile);
    }

    return {
        cleanupStagedAttachmentUrls: cleanupStagedAttachmentUrls,
        hasPendingStagedAttachments: hasPendingStagedAttachments,
        mergeDeltaAttachments: mergeDeltaAttachments,
        refreshAttachments: refreshAttachments,
        serializeStagedAttachments: serializeStagedAttachments,
        stripStagedAttachmentInternals: stripStagedAttachmentInternals
    };
});
