sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/AttachmentUploadPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/DetailRuntimeContracts"
], function (AttachmentUploadPolicy, DetailCommandPolicy, StatePaths, LayoutStateRuntime, ControllerViewStateRuntime, ModelStateRuntime, WorkflowContracts, ModelContracts, DetailRuntimeContracts) {
    "use strict";

    var ATTACHMENT_CONSTANTS = DetailRuntimeContracts.ATTACHMENTS;
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
        csv: "text/csv"
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

    function normalizeUploaderFiles(vFiles) {
        if (Array.isArray(vFiles)) {
            return vFiles.filter(Boolean);
        }
        if (vFiles && typeof vFiles.length === "number") {
            return Array.prototype.slice.call(vFiles).filter(Boolean);
        }
        return [];
    }

    function readUploaderFiles(oUploader) {
        var oFocusDomRef;
        if (oUploader && typeof oUploader.getFocusDomRef === "function") {
            oFocusDomRef = oUploader.getFocusDomRef();
            if (oFocusDomRef && oFocusDomRef.files && oFocusDomRef.files.length) {
                return normalizeUploaderFiles(oFocusDomRef.files);
            }
        }
        return [];
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
            return { ok: false, toastKey: ATTACHMENT_CONSTANTS.SIZE_REJECTED_TOAST_KEY };
        }
        if (aAllowedExtensions.length && sExtension && aAllowedExtensions.indexOf(sExtension) < 0) {
            return { ok: false, toastKey: ATTACHMENT_CONSTANTS.MIME_REJECTED_TOAST_KEY };
        }
        if (aAllowedMime.length && (!sMimeType || sMimeType === "application/octet-stream")) {
            return { ok: false, toastKey: ATTACHMENT_CONSTANTS.MIME_REJECTED_TOAST_KEY };
        }
        if (aAllowedMime.length && sMimeType && aAllowedMime.indexOf(sMimeType) < 0) {
            return { ok: false, toastKey: ATTACHMENT_CONSTANTS.MIME_REJECTED_TOAST_KEY };
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

    function openNativeFilePicker(oController) {
        var oUploader = oController.byId("attachmentUploader");

        if (!oUploader) {
            return Promise.resolve(false);
        }
        if (!canUploadAttachments(oController)) {
            oController._showToast(ATTACHMENT_CONSTANTS.UPLOAD_DISABLED_TOAST_KEY);
            return Promise.resolve(false);
        }
        if (oUploader && typeof oUploader.openFileDialog === "function") {
            oUploader.openFileDialog();
            return Promise.resolve(true);
        }
        return Promise.resolve(false);
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
            oController._showToast(ATTACHMENT_CONSTANTS.UPLOAD_DISABLED_TOAST_KEY);
            return Promise.resolve();
        }

        ControllerViewStateRuntime.setFlag(oController, ATTACHMENT_CONSTANTS.UPLOAD_BUSY_PATH, true);
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

        return Promise.resolve(oSequence).finally(function () {
            clearAttachmentUploader(oUploader);
            ControllerViewStateRuntime.setFlag(oController, ATTACHMENT_CONSTANTS.UPLOAD_BUSY_PATH, false);
        });
    }

    function onUploaderChange(oController, oEvent) {
        var oUploader = (oEvent && oEvent.getSource && oEvent.getSource()) || oController.byId("attachmentUploader");
        var aFiles = normalizeUploaderFiles(oEvent && oEvent.getParameter && oEvent.getParameter("files"));

        if (!aFiles.length) {
            aFiles = readUploaderFiles(oUploader);
        }
        if (aFiles.length) {
            return uploadFiles(oController, aFiles, oUploader);
        }
        clearAttachmentUploader(oUploader);
        return Promise.resolve();
    }

    function resolveTextFromBundle(oResourceBundle, sTextKey, aArgs, sFallbackText) {
        try {
            if (oResourceBundle && typeof oResourceBundle.getText === "function") {
                return String(oResourceBundle.getText(sTextKey, aArgs) || sFallbackText || sTextKey);
            }
        } catch (_bundleError) {
            // Fall back to the provided static text below.
        }
        return String(sFallbackText || sTextKey || "");
    }

    function formatUploadHint(oResourceBundle, aExtensions, iMaxSizeMb) {
        var sTypes = (Array.isArray(aExtensions) ? aExtensions : []).map(function (sExtension) {
            return String(sExtension || "").trim().toUpperCase();
        }).filter(Boolean).join(", ");
        var iSafeMaxSize = Number(iMaxSizeMb || 0) || 0;
        var sSize = String(iSafeMaxSize);
        var sFallbackText;

        if (!sTypes && iSafeMaxSize <= 0) {
            return "";
        }
        sFallbackText = [sTypes, iSafeMaxSize > 0 ? sSize + " MB" : ""].filter(Boolean).join(ATTACHMENT_CONSTANTS.HINT_FALLBACK_SEPARATOR);
        return resolveTextFromBundle(
            oResourceBundle,
            ATTACHMENT_CONSTANTS.HINT_TEXT_KEY,
            [sTypes || ATTACHMENT_CONSTANTS.HINT_FALLBACK_EMPTY_TOKEN, sSize],
            sFallbackText
        );
    }

    return {
        canUploadAttachments: canUploadAttachments,
        syncUploaderPolicy: syncUploaderPolicy,
        openNativeFilePicker: openNativeFilePicker,
        uploadFiles: uploadFiles,
        onUploaderChange: onUploaderChange,
        formatUploadHint: formatUploadHint
    };
});
