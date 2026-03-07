sap.ui.define([
    "sap_ui5/controller/support/AttachmentUploadCore",
    "sap_ui5/controller/support/AttachmentDropZoneRuntime"
], function (AttachmentUploadCore, AttachmentDropZoneRuntime) {
    "use strict";

    function ensureDropZoneDelegate(oController, oDropZone) {
        if (!oDropZone || !oDropZone.addEventDelegate || oController._attachmentDropZoneDelegate) {
            return;
        }
        oController._attachmentDropZoneDelegate = { onAfterRendering: function () { bindDropZone(oController); } };
        oDropZone.addEventDelegate(oController._attachmentDropZoneDelegate);
    }

    function clearDropZoneDelegate(oController, oDropZone) {
        if (oDropZone && oController._attachmentDropZoneDelegate && oDropZone.removeEventDelegate) {
            oDropZone.removeEventDelegate(oController._attachmentDropZoneDelegate);
            oController._attachmentDropZoneDelegate = null;
        }
    }

    function ensureHandlers(oController, aSpecs) {
        aSpecs.forEach(function (oSpec) {
            oController[oSpec.field] = oController[oSpec.field] || oSpec.handler.bind(null, oController);
        });
    }

    function toggleListeners(oTarget, aSpecs, oController, bAttach) {
        if (!oTarget) {
            return;
        }
        aSpecs.forEach(function (oSpec) {
            if (bAttach) {
                oTarget.addEventListener(oSpec.event, oController[oSpec.field], true);
                return;
            }
            oTarget.removeEventListener(oSpec.event, oController[oSpec.field], true);
        });
    }

    function bindDropZone(oController) {
        var oDropZone = oController.byId("attachmentDropZone");
        var oDomRef = oDropZone && oDropZone.getDomRef && oDropZone.getDomRef();

        AttachmentUploadCore.syncUploaderPolicy(oController);
        ensureDropZoneDelegate(oController, oDropZone);
        if (!oDomRef) {
            unbindDropZone(oController);
            return;
        }
        if (oController._attachmentDropZoneDom === oDomRef) {
            return;
        }

        unbindDropZone(oController);
        oController._attachmentDragDepth = 0;
        ensureHandlers(oController, AttachmentDropZoneRuntime.dropScopeSpecs);
        ensureHandlers(oController, AttachmentDropZoneRuntime.globalSpecs);
        toggleListeners(oDomRef, AttachmentDropZoneRuntime.dropScopeSpecs, oController, true);
        toggleListeners(document, AttachmentDropZoneRuntime.globalSpecs, oController, true);
        oController._attachmentDropZoneDom = oDomRef;
        oController._attachmentDropScopeDom = oDomRef;
    }

    function unbindDropZone(oController) {
        var oDropZone = oController.byId && oController.byId("attachmentDropZone");
        if (!oController._attachmentDropZoneDom) {
            clearDropZoneDelegate(oController, oDropZone);
            return;
        }
        if (oController._attachmentDropScopeDom) {
            toggleListeners(oController._attachmentDropScopeDom, AttachmentDropZoneRuntime.dropScopeSpecs, oController, false);
        }
        toggleListeners(document, AttachmentDropZoneRuntime.globalSpecs, oController, false);
        oController._attachmentDropZoneDom = null;
        oController._attachmentDropScopeDom = null;
        oController._attachmentDragDepth = 0;
        oController._attachmentGlobalDragDepth = 0;
        AttachmentDropZoneRuntime.resetVisual(oController);
        clearDropZoneDelegate(oController, oDropZone);
    }

    return {
        bindDropZone: bindDropZone,
        unbindDropZone: unbindDropZone,
        onUploaderChange: AttachmentUploadCore.onUploaderChange,
        formatUploadHint: AttachmentUploadCore.formatUploadHint,
        syncUploaderPolicy: AttachmentUploadCore.syncUploaderPolicy
    };
});
