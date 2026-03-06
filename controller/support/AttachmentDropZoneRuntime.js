sap.ui.define([
    "sap_ui5/controller/support/AttachmentUploadCore"
], function (AttachmentUploadCore) {
    "use strict";

    function setDropZoneClass(oController, sClassName, bActive) {
        var oDropZone = oController && oController.byId && oController.byId("attachmentDropZone");
        if (oDropZone && oDropZone.toggleStyleClass) {
            oDropZone.toggleStyleClass(sClassName, !!bActive);
        }
    }

    function setDropZoneState(oController, bActive) {
        setDropZoneClass(oController, "isAttachmentDropActive", bActive);
    }

    function setDropZonePrimedState(oController, bPrimed) {
        setDropZoneClass(oController, "isAttachmentDropPrimed", bPrimed);
    }

    function resetVisual(oController) {
        setDropZoneState(oController, false);
        setDropZonePrimedState(oController, false);
    }

    function hasFiles(oEvent) {
        var oTransfer = oEvent && oEvent.dataTransfer;
        var aTypes = oTransfer && oTransfer.types;
        if (!aTypes) {
            return false;
        }
        return Array.prototype.indexOf.call(aTypes, "Files") >= 0;
    }

    function isWithinDropScope(oController, oEvent) {
        var oDropZoneDom = oController && oController._attachmentDropZoneDom;
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

    function onAttachmentGlobalDragEnter(oController, oEvent) {
        if (!hasFiles(oEvent) || !AttachmentUploadCore.canUploadAttachments(oController)) {
            return;
        }
        oController._attachmentGlobalDragDepth = (oController._attachmentGlobalDragDepth || 0) + 1;
    }

    function onAttachmentGlobalDragOver(oController, oEvent) {
        var bAllowed;
        if (!hasFiles(oEvent) || !oEvent || !oEvent.dataTransfer) {
            return;
        }
        bAllowed = AttachmentUploadCore.canUploadAttachments(oController) && isWithinDropScope(oController, oEvent);
        oEvent.preventDefault();
        oEvent.stopPropagation();
        oEvent.dataTransfer.dropEffect = bAllowed ? "copy" : "none";
        setDropZonePrimedState(oController, bAllowed);
        setDropZoneState(oController, bAllowed);
    }

    function onAttachmentGlobalDragLeave(oController, oEvent) {
        if (!hasFiles(oEvent)) {
            return;
        }
        oController._attachmentGlobalDragDepth = Math.max(0, (oController._attachmentGlobalDragDepth || 1) - 1);
        if (oController._attachmentGlobalDragDepth === 0) {
            setDropZonePrimedState(oController, false);
        }
    }

    function onAttachmentGlobalDrop(oController, oEvent) {
        var aFiles = Array.prototype.slice.call((oEvent && oEvent.dataTransfer && oEvent.dataTransfer.files) || []);
        var bAllowed = hasFiles(oEvent) && AttachmentUploadCore.canUploadAttachments(oController) && isWithinDropScope(oController, oEvent);
        if (oEvent) {
            oEvent.preventDefault();
            oEvent.stopPropagation();
        }
        if (bAllowed && aFiles.length) {
            AttachmentUploadCore.uploadFiles(oController, aFiles, null);
        }
        oController._attachmentGlobalDragDepth = 0;
        resetVisual(oController);
    }

    function onAttachmentDragEnter(oController, oEvent) {
        oEvent.preventDefault();
        oEvent.stopPropagation();
        if (!AttachmentUploadCore.canUploadAttachments(oController)) {
            return;
        }
        oController._attachmentDragDepth = (oController._attachmentDragDepth || 0) + 1;
        setDropZonePrimedState(oController, true);
        setDropZoneState(oController, true);
    }

    function onAttachmentDragOver(oController, oEvent) {
        oEvent.preventDefault();
        oEvent.stopPropagation();
        if (oEvent.dataTransfer) {
            oEvent.dataTransfer.dropEffect = AttachmentUploadCore.canUploadAttachments(oController) ? "copy" : "none";
        }
        if (AttachmentUploadCore.canUploadAttachments(oController)) {
            setDropZoneState(oController, true);
            setDropZonePrimedState(oController, true);
        }
    }

    function onAttachmentDragLeave(oController, oEvent) {
        oEvent.preventDefault();
        oEvent.stopPropagation();
        oController._attachmentDragDepth = Math.max(0, (oController._attachmentDragDepth || 1) - 1);
        if (oController._attachmentDragDepth === 0) {
            resetVisual(oController);
        }
    }

    function onAttachmentDrop(oController, oEvent) {
        var aFiles;
        oEvent.preventDefault();
        oEvent.stopPropagation();
        oController._attachmentDragDepth = 0;
        oController._attachmentGlobalDragDepth = 0;
        resetVisual(oController);
        aFiles = Array.prototype.slice.call((oEvent.dataTransfer && oEvent.dataTransfer.files) || []);
        if (aFiles.length) {
            AttachmentUploadCore.uploadFiles(oController, aFiles, null);
        }
    }

    return {
        dropScopeSpecs: [
            { event: "dragenter", field: "_onAttachmentDragEnterBound", handler: onAttachmentDragEnter },
            { event: "dragover", field: "_onAttachmentDragOverBound", handler: onAttachmentDragOver },
            { event: "dragleave", field: "_onAttachmentDragLeaveBound", handler: onAttachmentDragLeave },
            { event: "drop", field: "_onAttachmentDropBound", handler: onAttachmentDrop }
        ],
        globalSpecs: [
            { event: "dragenter", field: "_onAttachmentGlobalDragEnterBound", handler: onAttachmentGlobalDragEnter },
            { event: "dragover", field: "_onAttachmentGlobalDragOverBound", handler: onAttachmentGlobalDragOver },
            { event: "dragleave", field: "_onAttachmentGlobalDragLeaveBound", handler: onAttachmentGlobalDragLeave },
            { event: "drop", field: "_onAttachmentGlobalDropBound", handler: onAttachmentGlobalDrop }
        ],
        resetVisual: resetVisual
    };
});
