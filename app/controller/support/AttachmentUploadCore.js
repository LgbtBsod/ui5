sap.ui.define([
    "checklist/app/util/CreateSentinel",
    "checklist/app/util/AttachmentUploadPolicy",
    "checklist/app/controller/support/DetailCommandPolicy",
    "checklist/app/controller/base/ControllerTextRuntime",
    "checklist/app/service/framework/LayoutStateRuntime",
    "checklist/app/controller/support/ControllerModelWriteSupport"
], function (CreateSentinel, AttachmentUploadPolicy, DetailCommandPolicy, ControllerTextRuntime, LayoutStateRuntime, ControllerModelWriteSupport) {
    "use strict";

    var ATTACHMENT_EXTENSION_TO_MIME = {
        jpg: "image/jpeg",
        jpeg: "image/jpeg",
        png: "image/png",
        pdf: "application/pdf",
        doc: "application/msword",
        docx: "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        xls: "application/vnd.ms-excel",
        xlsx: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        txt: "text/plain",
        csv: "text/csv",
        mp3: "audio/mpeg",
        wav: "audio/wav",
        m4a: "audio/mp4",
        ogg: "audio/ogg",
        aac: "audio/aac",
        flac: "audio/flac",
        webm: "audio/webm"
    };

    function fileExtension(sFileName) {
        var sName = String(sFileName || "").trim().toLowerCase();
        var iDot = sName.lastIndexOf(".");
        return iDot >= 0 ? sName.slice(iDot + 1) : "";
    }

    function resolveMimeType(oFile) {
        var sMimeType = String(oFile && oFile.type || "").trim().toLowerCase();
        var sExtension = fileExtension(oFile && oFile.name);
        if (sMimeType && sMimeType !== "application/octet-stream") {
            return sMimeType;
        }
        return ATTACHMENT_EXTENSION_TO_MIME[sExtension] || sMimeType || "application/octet-stream";
    }

    function getUploadPolicy(oController) {
        var oMasterDataModel = oController.getModel("masterData");
        return AttachmentUploadPolicy.normalizeUploadPolicy(
            (oMasterDataModel && oMasterDataModel.getProperty && oMasterDataModel.getProperty("/runtime/uploadPolicy")) || {}
        );
    }

    function canUploadAttachments(oController) {
        var sRootId = oController._currentRootId && oController._currentRootId();
        var sMode = LayoutStateRuntime.normalizeMode(ControllerModelWriteSupport.get(oController, "state", "/mode", "READ"), "READ");
        return !!sRootId && sMode !== "READ";
    }

    function setAttachmentBusy(oController, bBusy) {
        ControllerModelWriteSupport.set(oController, "view", "/attachmentBusy", !!bBusy);
    }

    function clearAttachmentUploader(oUploader) {
        if (oUploader && oUploader.clear) {
            oUploader.clear();
        }
    }

    function validateAttachmentFile(oController, oFile) {
        var oPolicy = getUploadPolicy(oController);
        var aAllowedMime = Array.isArray(oPolicy.allowedMime) ? oPolicy.allowedMime : [];
        var aAllowedExtensions = Array.isArray(oPolicy.allowedExtensions) ? oPolicy.allowedExtensions : [];
        var sMimeType = resolveMimeType(oFile);
        var sExtension = fileExtension(oFile && oFile.name);
        var iMaxSizeMb = Number(oPolicy.maxSizeMb || 0);
        var iFileSize = Number(oFile && oFile.size || 0) || 0;

        if (iMaxSizeMb > 0 && iFileSize > iMaxSizeMb * 1024 * 1024) {
            return { ok: false, toastKey: "attachmentSizeRejected" };
        }
        if (aAllowedExtensions.length && sExtension && aAllowedExtensions.indexOf(sExtension) < 0) {
            return { ok: false, toastKey: "attachmentMimeRejected" };
        }
        if (aAllowedMime.length && sMimeType && aAllowedMime.indexOf(sMimeType) < 0) {
            return { ok: false, toastKey: "attachmentMimeRejected" };
        }
        if (aAllowedMime.length && !sMimeType && !aAllowedExtensions.length) {
            return { ok: false, toastKey: "attachmentMimeRejected" };
        }
        return { ok: true, mimeType: sMimeType };
    }

    function buildAttachmentMeta(oController, oFile, sMimeType) {
        return {
            fileName: oFile.name || "",
            mimeType: sMimeType || resolveMimeType(oFile),
            fileSize: Number(oFile.size || 0) || 0,
            categoryKey: String(ControllerModelWriteSupport.get(oController, "view", "/attachmentCategoryKey", "GEN") || "GEN").trim() || "GEN"
        };
    }

    function syncUploaderPolicy(oController) {
        var oUploader = oController.byId("attachmentUploader");
        var oPolicy = getUploadPolicy(oController);
        if (!oUploader) {
            return;
        }
        if (oUploader.setMimeType) {
            oUploader.setMimeType(oPolicy.allowedMime || []);
        }
        if (oUploader.setFileType) {
            oUploader.setFileType(oPolicy.allowedExtensions || []);
        }
    }

    function uploadFiles(oController, aFiles, oUploader) {
        var sRootId = oController._currentRootId && oController._currentRootId();
        var aUploadFiles = (Array.isArray(aFiles) ? aFiles : []).filter(Boolean);
        var oSequence;

        if (!aUploadFiles.length) {
            clearAttachmentUploader(oUploader);
            return Promise.resolve();
        }
        if (!canUploadAttachments(oController)) {
            clearAttachmentUploader(oUploader);
            oController._showToast("attachmentUploadDisabled");
            return Promise.resolve();
        }

        setAttachmentBusy(oController, true);
        oSequence = aUploadFiles.reduce(function (oPromise, oFile) {
            return oPromise.then(function () {
                var oValidation = validateAttachmentFile(oController, oFile);
                if (!oValidation.ok) {
                    oController._showToast(oValidation.toastKey);
                    return Promise.resolve();
                }
                return DetailCommandPolicy.attachmentUpload(oController, {
                    rootId: sRootId,
                    file: oFile,
                    fileMeta: buildAttachmentMeta(oController, oFile, oValidation.mimeType)
                });
            });
        }, Promise.resolve());

        return oSequence.finally(function () {
            clearAttachmentUploader(oUploader);
            setAttachmentBusy(oController, false);
        });
    }

    function onUploaderChange(oController, oEvent) {
        var oUploader = (oEvent && oEvent.getSource && oEvent.getSource()) || oController.byId("attachmentUploader");
        var aFiles = oEvent && oEvent.getParameter && oEvent.getParameter("files");
        var oFile = (aFiles && aFiles[0]) || (oUploader && oUploader.FUEl && oUploader.FUEl.files && oUploader.FUEl.files[0]);
        if (oFile) {
            uploadFiles(oController, [oFile], oUploader);
        }
    }

    function formatUploadHint(oController, aExtensions, iMaxSizeMb) {
        var sTypes = (Array.isArray(aExtensions) ? aExtensions : []).map(function (sExtension) {
            return String(sExtension || "").trim().toUpperCase();
        }).filter(Boolean).join(", ");
        var sSize = String(Number(iMaxSizeMb || 0) || 0);
        if (!sTypes && sSize === "0") {
            return "";
        }
        return ControllerTextRuntime.getText(
            oController,
            "attachmentUploadHint",
            [sTypes || "-", sSize],
            [sTypes, sSize ? sSize + " MB" : ""].filter(Boolean).join(" · ")
        );
    }

    return {
        canUploadAttachments: canUploadAttachments,
        syncUploaderPolicy: syncUploaderPolicy,
        uploadFiles: uploadFiles,
        onUploaderChange: onUploaderChange,
        formatUploadHint: formatUploadHint
    };
});


