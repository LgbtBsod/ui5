sap.ui.define([
    "sap/ui/unified/FileUploaderParameter",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer"
], function (FileUploaderParameter, GatewayContractConstants, ODataKeyNormalizer) {
    "use strict";

    var ERROR_CODES = Object.freeze({
        UPLOADER_UNAVAILABLE: "attachment_uploader_unavailable",
        UPLOAD_FAILED: "attachment_upload_failed"
    });

    function normalizeDbKey(sValue) {
        return ODataKeyNormalizer.normalizeBinaryKey(sValue);
    }

    function resolveUploader(oController) {
        return oController && oController.byId && oController.byId("attachmentUploader");
    }

    function resolveServiceUrl(oController) {
        var oModel = oController && oController.getModel && oController.getModel("mainService");
        return String((oModel && oModel.sServiceUrl) || "").replace(/\/+$/, "");
    }

    function clearHeaderParameters(oUploader) {
        if (oUploader && typeof oUploader.removeAllHeaderParameters === "function") {
            oUploader.removeAllHeaderParameters();
        }
    }

    function addHeaderParameter(oUploader, sName, sValue) {
        if (!oUploader || typeof oUploader.addHeaderParameter !== "function") {
            return;
        }
        oUploader.addHeaderParameter(new FileUploaderParameter({
            name: sName,
            value: String(sValue || "")
        }));
    }

    function configureUploader(oUploader, sUploadUrl) {
        if (!oUploader) {
            return;
        }
        if (typeof oUploader.setUploadUrl === "function") {
            oUploader.setUploadUrl(sUploadUrl);
        }
        if (typeof oUploader.setSendXHR === "function") {
            oUploader.setSendXHR(true);
        }
        if (typeof oUploader.setUseMultipart === "function") {
            oUploader.setUseMultipart(false);
        }
        if (typeof oUploader.setMultiple === "function") {
            oUploader.setMultiple(false);
        }
        if (typeof oUploader.setSameFilenameAllowed === "function") {
            oUploader.setSameFilenameAllowed(true);
        }
    }

    function parseUploadResponse(oEvent) {
        var iStatus = Number(oEvent && oEvent.getParameter && oEvent.getParameter("status")) || 0;
        var sResponseRaw = String((oEvent && oEvent.getParameter && (oEvent.getParameter("responseRaw") || oEvent.getParameter("response"))) || "");
        if (iStatus < 200 || iStatus >= 300) {
            throw {
                statusCode: iStatus,
                code: ERROR_CODES.UPLOAD_FAILED,
                responseText: sResponseRaw
            };
        }
        if (!sResponseRaw) {
            return {};
        }
        try {
            return JSON.parse(sResponseRaw) || {};
        } catch (_parseError) {
            return { rawText: sResponseRaw };
        }
    }

    function uploadPendingAttachments(oController, mInput) {
        var oUploader = resolveUploader(oController);
        var oModel = oController && oController.getModel && oController.getModel("mainService");
        var sDbKey = normalizeDbKey(mInput && (mInput.dbKey || mInput.parentKey));
        var aAttachments = Array.isArray(mInput && mInput.attachments) ? mInput.attachments : [];
        var oAttachment = aAttachments[0] || null;
        var sServiceUrl = resolveServiceUrl(oController);
        var sCsrfToken = String((oModel && oModel.getSecurityToken && oModel.getSecurityToken()) || "").trim();
        var sFileName = String((oAttachment && oAttachment.fileName) || (oAttachment && oAttachment.file && oAttachment.file.name) || "").trim();
        var sUploadUrl = sServiceUrl + "/" + GatewayContractConstants.ENTITY_SETS.ATTACHMENT;
        var sParentKey = normalizeDbKey(oAttachment && (oAttachment.parentKey || oAttachment.dbKey)) || sDbKey;
        var sFolderKey = String((oAttachment && oAttachment.folderKey) || sParentKey).trim() || sParentKey;

        if (!oUploader || !sServiceUrl || !sDbKey || !sParentKey || !oAttachment) {
            return Promise.reject(new Error(ERROR_CODES.UPLOADER_UNAVAILABLE));
        }

        configureUploader(oUploader, sUploadUrl);
        clearHeaderParameters(oUploader);
        addHeaderParameter(oUploader, "X-CSRF-Token", sCsrfToken);
        addHeaderParameter(oUploader, "Slug", encodeURIComponent(sFileName || "attachment"));
        addHeaderParameter(oUploader, "X-DB-Key", sDbKey);
        addHeaderParameter(oUploader, "X-Parent-Key", sParentKey);
        addHeaderParameter(oUploader, "X-Folder-Key", sFolderKey);
        addHeaderParameter(oUploader, "X-Category-Key", String((oAttachment && oAttachment.categoryKey) || "GEN").trim() || "GEN");
        addHeaderParameter(oUploader, "X-Description", String((oAttachment && oAttachment.description) || "").trim());
        addHeaderParameter(oUploader, "X-File-Name", sFileName || "attachment");

        return new Promise(function (resolve, reject) {
            var fnComplete = function (oEvent) {
                try {
                    resolve([parseUploadResponse(oEvent)]);
                } catch (oError) {
                    reject(oError);
                } finally {
                    clearHeaderParameters(oUploader);
                    if (typeof oUploader.clear === "function") {
                        oUploader.clear();
                    }
                    if (typeof oUploader.detachUploadComplete === "function") {
                        oUploader.detachUploadComplete(fnComplete);
                    }
                }
            };

            if (typeof oUploader.attachUploadComplete === "function") {
                oUploader.attachUploadComplete(fnComplete);
            }
            try {
                oUploader.upload();
            } catch (oUploadError) {
                if (typeof oUploader.detachUploadComplete === "function") {
                    oUploader.detachUploadComplete(fnComplete);
                }
                clearHeaderParameters(oUploader);
                reject(oUploadError);
            }
        });
    }

    function createGateway(oController) {
        return {
            uploadPendingAttachments: function (mInput) {
                return uploadPendingAttachments(oController, mInput || {});
            }
        };
    }

    return {
        createGateway: createGateway,
        uploadPendingAttachments: uploadPendingAttachments
    };
});
