sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore"
], function (AttachmentUploadCore) {
    "use strict";

    function hasFiles(oEvent) {
        var oTransfer = oEvent && oEvent.dataTransfer;
        var aTypes = oTransfer && oTransfer.types;
        var aFiles = oTransfer && oTransfer.files;
        if (aFiles && aFiles.length) {
            return true;
        }
        if (!aTypes) {
            return false;
        }
        return Array.prototype.indexOf.call(aTypes, "Files") >= 0
            || Array.prototype.indexOf.call(aTypes, "application/x-moz-file") >= 0;
    }

    function extractFiles(oEvent) {
        return Array.prototype.slice.call((oEvent && oEvent.dataTransfer && oEvent.dataTransfer.files) || []);
    }

    function isWithinDropScope(oController, oEvent) {
        var oDropZoneDom = (oController && oController._attachmentDropScopeDom) || (oController && oController._attachmentDropZoneDom);
        var oTarget = oEvent && oEvent.target;
        var iX = Number(oEvent && oEvent.clientX);
        var iY = Number(oEvent && oEvent.clientY);
        var oRect;
        if (oDropZoneDom && oTarget && oDropZoneDom.contains && oDropZoneDom.contains(oTarget)) {
            return true;
        }
        if (!oDropZoneDom || !Number.isFinite(iX) || !Number.isFinite(iY) || !oDropZoneDom.getBoundingClientRect) {
            return false;
        }
        oRect = oDropZoneDom.getBoundingClientRect();
        return iX >= oRect.left && iX <= oRect.right && iY >= oRect.top && iY <= oRect.bottom;
    }

    function canUpload(oController) {
        return AttachmentUploadCore.canUploadAttachments(oController);
    }

    function canHandleDrop(oController, oEvent) {
        return hasFiles(oEvent) && canUpload(oController);
    }

    function isAllowedDrop(oController, oEvent) {
        return canHandleDrop(oController, oEvent) && isWithinDropScope(oController, oEvent);
    }

    function dispatchUpload(oController, oEvent) {
        var aFiles = extractFiles(oEvent);
        if (aFiles.length) {
            AttachmentUploadCore.uploadFiles(oController, aFiles, null);
        }
    }

    return {
        canHandleDrop: canHandleDrop,
        canUpload: canUpload,
        dispatchUpload: dispatchUpload,
        extractFiles: extractFiles,
        hasFiles: hasFiles,
        isAllowedDrop: isAllowedDrop
    };
});
