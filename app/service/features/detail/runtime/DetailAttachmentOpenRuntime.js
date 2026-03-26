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

    function resolveAttachmentContext(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return (oSource && oSource.getBindingContext && (oSource.getBindingContext(DETAIL_MODEL) || oSource.getBindingContext(VIEW_MODEL))) || null;
    }

    function openAttachment(oController, oEvent) {
        var oCtx = resolveAttachmentContext(oEvent);
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        var oMainService = oController && oController.getModel && oController.getModel("mainService");
        var sFileName = String((oRow && oRow.FileName) || "attachment").trim() || "attachment";
        var sEntityPath;
        var sDownloadUrl = String((oRow && oRow.DownloadUrl) || "").trim();
        var sDocumentHandle = String((oRow && oRow.DocumentHandle) || "").trim();

        if (sDownloadUrl) {
            return Promise.resolve(DownloadRuntime.triggerDownload(sDownloadUrl, sFileName));
        }
        if (sDocumentHandle) {
            return Promise.resolve(DownloadRuntime.triggerDownload(sDocumentHandle, sFileName));
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
                    var sResolvedDownloadUrl = String((oData && oData.DownloadUrl) || "").trim();
                    var sResolvedDocumentHandle = String((oData && oData.DocumentHandle) || "").trim();
                    if (sResolvedDownloadUrl) {
                        resolve(DownloadRuntime.triggerDownload(sResolvedDownloadUrl, sFileName));
                        return;
                    }
                    if (sResolvedDocumentHandle) {
                        resolve(DownloadRuntime.triggerDownload(sResolvedDocumentHandle, sFileName));
                        return;
                    }
                    resolve(false);
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
