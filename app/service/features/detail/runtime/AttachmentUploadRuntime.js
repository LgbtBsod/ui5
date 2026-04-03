sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/AttachmentUploadPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (AttachmentUploadPolicy, StatePaths, LayoutStateRuntime, ControllerViewStateRuntime, ModelStateRuntime, WorkflowContracts, ModelContracts, DetailRuntimeContracts, MessageKeyConstants, CreateSentinel) {
    "use strict";

    var ATTACHMENT_CONSTANTS = DetailRuntimeContracts.ATTACHMENTS;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;
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
        var sCurrentDbKey = oController._currentChecklistDbKey && oController._currentChecklistDbKey();
        var sMode = LayoutStateRuntime.normalizeMode(ModelStateRuntime.read(oController, STATE_MODEL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ), WorkflowContracts.EDIT_MODES.READ);
        return !!sCurrentDbKey && !CreateSentinel.isCreateId(sCurrentDbKey) && sMode !== WorkflowContracts.EDIT_MODES.READ;
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

    function runDetailCommand(oController, sMethod, mInput) {
        if (!oController || typeof oController._dispatchDetailCommand !== "function") {
            return Promise.resolve(false);
        }
        return oController._dispatchDetailCommand(sMethod, mInput || {});
    }

    function uploadPersistedFiles(oController, sDbKey, aFiles, mHooks) {
        var oAttachmentGateway = mHooks.attachmentGateway;
        var oFile = aFiles[0];
        var oValidation;
        var oSpec;

        if (!oFile || !oAttachmentGateway || typeof oAttachmentGateway.uploadPendingAttachments !== "function") {
            return Promise.resolve();
        }
        oValidation = validateAttachmentFile(oController, oFile);
        if (!oValidation.ok) {
            mHooks.showToast(oValidation.toastKey);
            return Promise.resolve();
        }
        oSpec = Object.assign({ file: oFile, dbKey: sDbKey, parentKey: sDbKey }, buildAttachmentMeta(oController, oFile, oValidation.mimeType));
        return oAttachmentGateway.uploadPendingAttachments({
            dbKey: sDbKey,
            parentKey: sDbKey,
            attachments: [oSpec]
        }).then(function () {
            mHooks.showToast(DETAIL_MESSAGE_KEYS.ATTACHMENT_UPLOADED);
            return runDetailCommand(oController, "attachmentLoad", { dbKey: sDbKey });
        });
    }

    function uploadFiles(oController, aFiles, mHooks) {
        var sCurrentDbKey = oController._currentChecklistDbKey && oController._currentChecklistDbKey();
        var aUploadFiles = (Array.isArray(aFiles) ? aFiles : []).filter(Boolean);
        var oSequence;

        if (!aUploadFiles.length) {
            mHooks.clearUploader();
            return Promise.resolve();
        }
        if (!canUploadAttachments(oController)) {
            mHooks.clearUploader();
            mHooks.showToast(ATTACHMENT_CONSTANTS.UPLOAD_DISABLED_TOAST_KEY);
            return Promise.resolve();
        }

        ControllerViewStateRuntime.setFlag(oController, ATTACHMENT_CONSTANTS.UPLOAD_BUSY_PATH, true);
        oSequence = CreateSentinel.isCreateId(sCurrentDbKey)
            ? aUploadFiles.reduce(function (oPromise, oFile) {
                return oPromise.then(function () {
                    var oValidation = validateAttachmentFile(oController, oFile);
                    if (!oValidation.ok) {
                        mHooks.showToast(oValidation.toastKey);
                        return Promise.resolve();
                    }
                    return runDetailCommand(oController, "attachmentUpload", {
                        dbKey: sCurrentDbKey,
                        file: oFile,
                        fileMeta: buildAttachmentMeta(oController, oFile, oValidation.mimeType)
                    });
                });
            }, Promise.resolve())
            : uploadPersistedFiles(oController, sCurrentDbKey, aUploadFiles, mHooks);

        return Promise.resolve(oSequence).finally(function () {
            mHooks.clearUploader();
            ControllerViewStateRuntime.setFlag(oController, ATTACHMENT_CONSTANTS.UPLOAD_BUSY_PATH, false);
        });
    }

    return {
        canUploadAttachments: canUploadAttachments,
        getUploadPolicy: getUploadPolicy,
        uploadFiles: uploadFiles
    };
});
