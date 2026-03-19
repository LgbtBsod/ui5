sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFieldContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataKeyContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (ModelStateRuntime, ModelContracts, DetailFieldContracts, ODataAdapterUtils, ODataKeyContracts, CreateSentinel) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var VIEW_MODEL = MODELS.VIEW;
    var SELECTED_MODEL = MODELS.SELECTED;
    var VIEW_PATHS = DetailFieldContracts.VIEW_PATHS;

    function base64ToBlob(sBase64, sMimeType) {
        var sBinary = atob(String(sBase64 || "").trim());
        var iLength = sBinary.length;
        var aBytes = new Uint8Array(iLength);
        var iIndex;
        for (iIndex = 0; iIndex < iLength; iIndex += 1) {
            aBytes[iIndex] = sBinary.charCodeAt(iIndex);
        }
        return new Blob([aBytes], {
            type: String(sMimeType || "application/octet-stream").trim() || "application/octet-stream"
        });
    }

    function resolveAttachmentContext(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return (oSource && oSource.getBindingContext && (oSource.getBindingContext(SELECTED_MODEL) || oSource.getBindingContext(VIEW_MODEL))) || null;
    }

    function deleteAttachment(oController, oEvent, mHooks) {
        var oCtx = resolveAttachmentContext(oEvent);
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        if (!sAttachmentId) {
            return Promise.resolve(false);
        }
        return ModelStateRuntime.withFlag(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENT_BUSY, function () {
            return mHooks.attachmentDelete({
                attachmentId: sAttachmentId,
                attachment: oRow || null
            });
        });
    }

    function toggleAttachmentsSection(oController, mHooks) {
        var bExpanded = !!ModelStateRuntime.read(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_EXPANDED, false);
        var bLoaded = !!ModelStateRuntime.read(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_LOADED, false);
        var aSessionAttachments = ModelStateRuntime.read(oController, VIEW_MODEL, VIEW_PATHS.SESSION_ATTACHMENTS, []) || [];
        var sRootId = String((oController && oController._currentRootId && oController._currentRootId()) || "").trim();
        if (bExpanded) {
            ModelStateRuntime.write(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_EXPANDED, false);
            mHooks.unbindAttachmentDropZone();
            return Promise.resolve({ collapsed: true });
        }
        ModelStateRuntime.write(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_EXPANDED, true);
        mHooks.scheduleAttachmentDropZoneBind();
        if (CreateSentinel.isCreateId(sRootId)) {
            ModelStateRuntime.write(oController, SELECTED_MODEL, "/attachments", aSessionAttachments);
            ModelStateRuntime.write(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_LOADED, true);
            return Promise.resolve({ expanded: true, staged: true });
        }
        if (bLoaded) {
            return Promise.resolve({ expanded: true, loaded: true });
        }
        return ModelStateRuntime.withFlag(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENT_BUSY, function () {
            return mHooks.attachmentLoad();
        });
    }

    function openAttachment(oController, oEvent) {
        var oCtx = resolveAttachmentContext(oEvent);
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        var sLocalObjectUrl = String((oRow && oRow.localObjectUrl) || "").trim();
        var oMainService = oController && oController.getModel && oController.getModel("mainService");
        var sFileName = String((oRow && oRow.FileName) || "attachment").trim() || "attachment";
        var oLink;
        var sEntityPath;

        function triggerDownload(sUrl) {
            if (!sUrl) {
                return false;
            }
            oLink = document.createElement("a");
            oLink.href = sUrl;
            oLink.download = sFileName;
            oLink.rel = "noopener";
            oLink.style.display = "none";
            document.body.appendChild(oLink);
            oLink.click();
            document.body.removeChild(oLink);
            return true;
        }

        if (sLocalObjectUrl) {
            return triggerDownload(sLocalObjectUrl);
        }
        if (!sAttachmentId || !oMainService || typeof oMainService.read !== "function") {
            return Promise.resolve(false);
        }
        sEntityPath = ODataAdapterUtils.buildEntityPath("AttachmentSet", sAttachmentId, {
            name: "AttachmentKey",
            type: ODataKeyContracts.TYPES.ATTACHMENT_KEY
        });
        return new Promise(function (resolve) {
            oMainService.read(sEntityPath, {
                urlParameters: {
                    "$select": "AttachmentKey,FileName,MimeType,Value"
                },
                success: function (oData) {
                    var sValue = String((oData && oData.Value) || "").trim();
                    var oBlob;
                    var sObjectUrl;
                    if (!sValue) {
                        resolve(false);
                        return;
                    }
                    oBlob = base64ToBlob(sValue, oData && oData.MimeType);
                    sObjectUrl = window.URL.createObjectURL(oBlob);
                    triggerDownload(sObjectUrl);
                    window.setTimeout(function () {
                        window.URL.revokeObjectURL(sObjectUrl);
                    }, 0);
                    resolve(true);
                },
                error: function () {
                    resolve(false);
                }
            });
        });
    }

    return {
        deleteAttachment: deleteAttachment,
        openAttachment: openAttachment,
        toggleAttachmentsSection: toggleAttachmentsSection
    };
});
