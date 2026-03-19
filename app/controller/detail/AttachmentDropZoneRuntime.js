sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentDropZoneEventRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentDropZoneVisualRuntime"
], function (AttachmentDropZoneEventRuntime, AttachmentDropZoneVisualRuntime) {
    "use strict";

    function resetVisual(oController) {
        AttachmentDropZoneVisualRuntime.resetVisual(oController);
    }

    function onAttachmentGlobalDragEnter(oController, oEvent) {
        if (!AttachmentDropZoneEventRuntime.canHandleDrop(oController, oEvent)) {
            return;
        }
        oController._attachmentGlobalDragDepth = (oController._attachmentGlobalDragDepth || 0) + 1;
    }

    function onAttachmentGlobalDragOver(oController, oEvent) {
        var bAllowed;
        if (!AttachmentDropZoneEventRuntime.hasFiles(oEvent) || !oEvent || !oEvent.dataTransfer) {
            return;
        }
        bAllowed = AttachmentDropZoneEventRuntime.isAllowedDrop(oController, oEvent);
        oEvent.preventDefault();
        oEvent.stopPropagation();
        oEvent.dataTransfer.dropEffect = bAllowed ? "copy" : "none";
        AttachmentDropZoneVisualRuntime.setDropZoneState(oController, bAllowed, bAllowed);
    }

    function onAttachmentGlobalDragLeave(oController, oEvent) {
        if (!AttachmentDropZoneEventRuntime.hasFiles(oEvent)) {
            return;
        }
        oController._attachmentGlobalDragDepth = Math.max(0, (oController._attachmentGlobalDragDepth || 1) - 1);
        if (oController._attachmentGlobalDragDepth === 0) {
            AttachmentDropZoneVisualRuntime.setDropZoneState(oController, false, false);
        }
    }

    function onAttachmentGlobalDrop(oController, oEvent) {
        var bAllowed = AttachmentDropZoneEventRuntime.isAllowedDrop(oController, oEvent);
        if (oEvent) {
            oEvent.preventDefault();
            oEvent.stopPropagation();
        }
        if (bAllowed) {
            AttachmentDropZoneEventRuntime.dispatchUpload(oController, oEvent);
        }
        oController._attachmentGlobalDragDepth = 0;
        resetVisual(oController);
    }

    function onAttachmentDragEnter(oController, oEvent) {
        oEvent.preventDefault();
        oEvent.stopPropagation();
        if (!AttachmentDropZoneEventRuntime.canUpload(oController)) {
            return;
        }
        oController._attachmentDragDepth = (oController._attachmentDragDepth || 0) + 1;
        AttachmentDropZoneVisualRuntime.setDropZoneState(oController, true, true);
    }

    function onAttachmentDragOver(oController, oEvent) {
        oEvent.preventDefault();
        oEvent.stopPropagation();
        if (oEvent.dataTransfer) {
            oEvent.dataTransfer.dropEffect = AttachmentDropZoneEventRuntime.canUpload(oController) ? "copy" : "none";
        }
        if (AttachmentDropZoneEventRuntime.canUpload(oController)) {
            AttachmentDropZoneVisualRuntime.setDropZoneState(oController, true, true);
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
        oEvent.preventDefault();
        oEvent.stopPropagation();
        oController._attachmentDragDepth = 0;
        oController._attachmentGlobalDragDepth = 0;
        resetVisual(oController);
        AttachmentDropZoneEventRuntime.dispatchUpload(oController, oEvent);
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
