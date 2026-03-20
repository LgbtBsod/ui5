sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentDropZoneRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EventDelegateRuntime"
], function (AttachmentUploadCore, AttachmentDropZoneRuntime, SchedulingRuntime, EventDelegateRuntime) {
    "use strict";

    function ensureDropZoneDelegate(oController, oDropZone) {
        if (!oDropZone || !oDropZone.addEventDelegate) {
            return;
        }
        if (!oController._attachmentDropZoneDelegate) {
            oController._attachmentDropZoneDelegate = {
                onAfterRendering: function () {
                    bindAttachmentDropZone(oController);
                }
            };
        }
        EventDelegateRuntime.ensure(oController, "_attachmentDropZoneDelegate", oDropZone, oController._attachmentDropZoneDelegate, oController);
    }

    function clearDropZoneDelegate(oController, oDropZone) {
        EventDelegateRuntime.remove(oController, "_attachmentDropZoneDelegate", oDropZone);
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

    function bindAttachmentDropZone(oController) {
        var oDropZone = oController.byId("attachmentDropZone");
        var oScope = oController.byId("attachmentDropScope") || oDropZone;
        var oDomRef = oDropZone && oDropZone.getDomRef && oDropZone.getDomRef();
        var oScopeDom = oScope && oScope.getDomRef && oScope.getDomRef();

        AttachmentUploadCore.syncUploaderPolicy(oController);
        ensureDropZoneDelegate(oController, oDropZone);
        if (!oDomRef) {
            unbindAttachmentDropZone(oController);
            return;
        }
        if (oController._attachmentDropZoneDom === oDomRef && oController._attachmentDropScopeDom === (oScopeDom || oDomRef)) {
            return;
        }

        unbindAttachmentDropZone(oController);
        oController._attachmentDragDepth = 0;
        ensureHandlers(oController, AttachmentDropZoneRuntime.dropScopeSpecs);
        ensureHandlers(oController, AttachmentDropZoneRuntime.globalSpecs);
        toggleListeners(oScopeDom || oDomRef, AttachmentDropZoneRuntime.dropScopeSpecs, oController, true);
        toggleListeners(document, AttachmentDropZoneRuntime.globalSpecs, oController, true);
        toggleListeners(window, AttachmentDropZoneRuntime.globalSpecs, oController, true);
        oController._attachmentDropZoneDom = oDomRef;
        oController._attachmentDropScopeDom = oScopeDom || oDomRef;
    }

    function unbindAttachmentDropZone(oController) {
        var oDropZone = oController.byId && oController.byId("attachmentDropZone");
        if (!oController._attachmentDropZoneDom) {
            clearDropZoneDelegate(oController, oDropZone);
            return;
        }
        if (oController._attachmentDropScopeDom) {
            toggleListeners(oController._attachmentDropScopeDom, AttachmentDropZoneRuntime.dropScopeSpecs, oController, false);
        }
        toggleListeners(document, AttachmentDropZoneRuntime.globalSpecs, oController, false);
        toggleListeners(window, AttachmentDropZoneRuntime.globalSpecs, oController, false);
        oController._attachmentDropZoneDom = null;
        oController._attachmentDropScopeDom = null;
        oController._attachmentDragDepth = 0;
        oController._attachmentGlobalDragDepth = 0;
        AttachmentDropZoneRuntime.resetVisual(oController);
        clearDropZoneDelegate(oController, oDropZone);
    }

    function scheduleAttachmentDropZoneBind(oController, iAttempt) {
        var iNextAttempt = Number(iAttempt || 0);
        oController._iAttachmentDropZoneBindTimer = SchedulingRuntime.restartTimer(oController._iAttachmentDropZoneBindTimer, function () {
            var oDropZone;
            var oDropZoneDom;
            oController._iAttachmentDropZoneBindTimer = null;
            AttachmentUploadCore.syncUploaderPolicy(oController);
            oDropZone = oController.byId("attachmentDropZone");
            oDropZoneDom = oDropZone && oDropZone.getDomRef && oDropZone.getDomRef();
            if (!oDropZoneDom) {
                unbindAttachmentDropZone(oController);
            } else {
                bindAttachmentDropZone(oController);
            }
            if (!oDropZoneDom && iNextAttempt < 8) {
                scheduleAttachmentDropZoneBind(oController, iNextAttempt + 1);
            }
        }, iNextAttempt === 0 ? 0 : 180);
    }

    return {
        bindAttachmentDropZone: bindAttachmentDropZone,
        scheduleAttachmentDropZoneBind: scheduleAttachmentDropZoneBind,
        unbindAttachmentDropZone: unbindAttachmentDropZone
    };
});
