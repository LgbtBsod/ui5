sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/AttachmentUploadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds"
], function (AttachmentUploadRuntime, DetailRuntimeContracts, UiControlIds) {
    "use strict";

    var ATTACHMENT_CONSTANTS = DetailRuntimeContracts.ATTACHMENTS;
    var ATTACHMENT_IDS = UiControlIds.DETAIL;

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

    function syncUploaderPolicy(oController) {
        var oUploader = oController.byId(ATTACHMENT_IDS.ATTACHMENT_UPLOADER);
        var oPolicy = AttachmentUploadRuntime.getUploadPolicy(oController);
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
        var oUploader = oController.byId(ATTACHMENT_IDS.ATTACHMENT_UPLOADER);

        if (!oUploader) {
            return Promise.resolve(false);
        }
        if (!AttachmentUploadRuntime.canUploadAttachments(oController)) {
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
        return AttachmentUploadRuntime.uploadFiles(oController, aFiles, {
            clearUploader: function () {
                clearAttachmentUploader(oUploader);
            },
            showToast: function (sToastKey) {
                oController._showToast(sToastKey);
            }
        });
    }

    function onUploaderChange(oController, oEvent) {
        var oUploader = (oEvent && oEvent.getSource && oEvent.getSource()) || oController.byId(ATTACHMENT_IDS.ATTACHMENT_UPLOADER);
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
        canUploadAttachments: AttachmentUploadRuntime.canUploadAttachments,
        syncUploaderPolicy: syncUploaderPolicy,
        openNativeFilePicker: openNativeFilePicker,
        uploadFiles: uploadFiles,
        onUploaderChange: onUploaderChange,
        formatUploadHint: formatUploadHint
    };
});
