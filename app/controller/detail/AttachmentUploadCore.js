sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/util/AttachmentUploadPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DetailRuntimeContracts"
], function (CreateSentinel, AttachmentUploadPolicy, DetailCommandPolicy, ControllerTextRuntime, StatePaths, LayoutStateRuntime, ControllerViewStateRuntime, ModelStateRuntime, WorkflowContracts, ModelContracts, DetailRuntimeContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MASTER_DATA_MODEL = MODELS.MASTER_DATA;
    var STATE_MODEL = MODELS.STATE;
    var VIEW_DEFAULTS = DetailRuntimeContracts.VIEW_DEFAULTS;

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
        return AttachmentUploadPolicy.normalizeUploadPolicy(
            ModelStateRuntime.read(oController, MASTER_DATA_MODEL, "/runtime/uploadPolicy", {}) || {}
        );
    }

    function canUploadAttachments(oController) {
        var sRootId = oController._currentRootId && oController._currentRootId();
        var sMode = LayoutStateRuntime.normalizeMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ), WorkflowContracts.EDIT_MODES.READ);
        return !!sRootId && sMode !== WorkflowContracts.EDIT_MODES.READ;
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
            categoryKey: String(ControllerViewStateRuntime.get(oController, "/attachmentCategoryKey", VIEW_DEFAULTS.ATTACHMENT_CATEGORY_KEY) || VIEW_DEFAULTS.ATTACHMENT_CATEGORY_KEY).trim() || VIEW_DEFAULTS.ATTACHMENT_CATEGORY_KEY
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

        ControllerViewStateRuntime.setFlag(oController, "/attachmentBusy", true);
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
            ControllerViewStateRuntime.setFlag(oController, "/attachmentBusy", false);
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
            [sTypes, sSize ? sSize + " MB" : ""].filter(Boolean).join(" Â· ")
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


