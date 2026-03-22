sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DownloadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (ODataAdapterUtils, ODataKeyContracts, ModelContracts, DownloadRuntime, GatewayContractConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var VIEW_MODEL = MODELS.VIEW;
    var DETAIL_MODEL = MODELS.DETAIL;

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
        return (oSource && oSource.getBindingContext && (oSource.getBindingContext(DETAIL_MODEL) || oSource.getBindingContext(VIEW_MODEL))) || null;
    }

    function openAttachment(oController, oEvent) {
        var oCtx = resolveAttachmentContext(oEvent);
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        var sLocalObjectUrl = String((oRow && oRow.localObjectUrl) || "").trim();
        var oMainService = oController && oController.getModel && oController.getModel("mainService");
        var sFileName = String((oRow && oRow.FileName) || "attachment").trim() || "attachment";
        var sEntityPath;

        if (sLocalObjectUrl) {
            return DownloadRuntime.triggerDownload(sLocalObjectUrl, sFileName);
        }
        if (!sAttachmentId || !oMainService || typeof oMainService.read !== "function") {
            return Promise.resolve(false);
        }
        sEntityPath = ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.ATTACHMENT, sAttachmentId, {
            name: "AttachmentKey",
            type: ODataKeyContracts.TYPES.ATTACHMENT_KEY
        });
        return new Promise(function (resolve) {
            oMainService.read(sEntityPath, {
                urlParameters: {
                    "$select": ODataKeyContracts.SELECTS.ATTACHMENT_CONTENT
                },
                success: function (oData) {
                    var sValue = String((oData && oData.Value) || "").trim();
                    var oBlob;
                    if (!sValue) {
                        resolve(false);
                        return;
                    }
                    oBlob = base64ToBlob(sValue, oData && oData.MimeType);
                    resolve(DownloadRuntime.withObjectUrl(oBlob, function (sObjectUrl) {
                        return DownloadRuntime.triggerDownload(sObjectUrl, sFileName);
                    }));
                },
                error: function () {
                    resolve(false);
                }
            });
        });
    }

    return {
        openAttachment: openAttachment
    };
});
