sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EventDelegateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControlStyleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiControlIds"
], function (AttachmentUploadCore, SchedulingRuntime, EventDelegateRuntime, ControlStyleRuntime, DetailContracts, UiControlIds) {
    "use strict";

    var ATTACHMENT_CONSTANTS = DetailContracts.ATTACHMENTS;
    var ATTACHMENT_IDS = UiControlIds.DETAIL;

    function setDropZoneClass(oController, sClassName, bActive) {
        var oDropZone = oController && oController.byId && oController.byId(ATTACHMENT_IDS.ATTACHMENT_DROP_ZONE);
        if (!oDropZone) {
            return;
        }
        if (bActive) {
            ControlStyleRuntime.enable(oDropZone, sClassName);
            return;
        }
        ControlStyleRuntime.disable(oDropZone, sClassName);
    }

    function setDropZoneState(oController, bPrimed, bActive) {
        setDropZoneClass(oController, ATTACHMENT_CONSTANTS.CSS_CLASSES.DROP_PRIMED, !!bPrimed);
        setDropZoneClass(oController, ATTACHMENT_CONSTANTS.CSS_CLASSES.DROP_ACTIVE, !!bActive);
    }

    function resetVisual(oController) {
        setDropZoneState(oController, false, false);
    }

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
        return Array.prototype.indexOf.call(aTypes, ATTACHMENT_CONSTANTS.DOM_FILE_TYPE_STANDARD) >= 0
            || Array.prototype.indexOf.call(aTypes, ATTACHMENT_CONSTANTS.DOM_FILE_TYPE_MOZ) >= 0;
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
        var oDropZone = oController.byId(ATTACHMENT_IDS.ATTACHMENT_DROP_ZONE);
        var oScope = oController.byId(ATTACHMENT_IDS.ATTACHMENT_DROP_SCOPE) || oDropZone;
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
        var oDropZone = oController.byId && oController.byId(ATTACHMENT_IDS.ATTACHMENT_DROP_ZONE);
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
        resetVisual(oController);
        clearDropZoneDelegate(oController, oDropZone);
    }

    function scheduleAttachmentDropZoneBind(oController, iAttempt) {
        var iNextAttempt = Number(iAttempt || 0);
        oController._iAttachmentDropZoneBindTimer = SchedulingRuntime.restartTimer(oController._iAttachmentDropZoneBindTimer, function () {
            var oDropZone;
            var oDropZoneDom;
            oController._iAttachmentDropZoneBindTimer = null;
            AttachmentUploadCore.syncUploaderPolicy(oController);
            oDropZone = oController.byId(ATTACHMENT_IDS.ATTACHMENT_DROP_ZONE);
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

    function onAttachmentGlobalDragEnter(oController, oEvent) {
        if (!canHandleDrop(oController, oEvent)) {
            return;
        }
        oController._attachmentGlobalDragDepth = (oController._attachmentGlobalDragDepth || 0) + 1;
    }

    function onAttachmentGlobalDragOver(oController, oEvent) {
        var bAllowed;
        if (!hasFiles(oEvent) || !oEvent || !oEvent.dataTransfer) {
            return;
        }
        bAllowed = isAllowedDrop(oController, oEvent);
        oEvent.preventDefault();
        oEvent.stopPropagation();
        oEvent.dataTransfer.dropEffect = bAllowed ? "copy" : "none";
        setDropZoneState(oController, bAllowed, bAllowed);
    }

    function onAttachmentGlobalDragLeave(oController, oEvent) {
        if (!hasFiles(oEvent)) {
            return;
        }
        oController._attachmentGlobalDragDepth = Math.max(0, (oController._attachmentGlobalDragDepth || 1) - 1);
        if (oController._attachmentGlobalDragDepth === 0) {
            setDropZoneState(oController, false, false);
        }
    }

    function onAttachmentGlobalDrop(oController, oEvent) {
        var bAllowed = isAllowedDrop(oController, oEvent);
        if (oEvent) {
            oEvent.preventDefault();
            oEvent.stopPropagation();
        }
        if (bAllowed) {
            dispatchUpload(oController, oEvent);
        }
        oController._attachmentGlobalDragDepth = 0;
        resetVisual(oController);
    }

    function onAttachmentDragEnter(oController, oEvent) {
        oEvent.preventDefault();
        oEvent.stopPropagation();
        if (!canUpload(oController)) {
            return;
        }
        oController._attachmentDragDepth = (oController._attachmentDragDepth || 0) + 1;
        setDropZoneState(oController, true, true);
    }

    function onAttachmentDragOver(oController, oEvent) {
        oEvent.preventDefault();
        oEvent.stopPropagation();
        if (oEvent.dataTransfer) {
            oEvent.dataTransfer.dropEffect = canUpload(oController) ? "copy" : "none";
        }
        if (canUpload(oController)) {
            setDropZoneState(oController, true, true);
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
        dispatchUpload(oController, oEvent);
    }

    var AttachmentDropZoneRuntime = {
        bindAttachmentDropZone: bindAttachmentDropZone,
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
        resetVisual: resetVisual,
        scheduleAttachmentDropZoneBind: scheduleAttachmentDropZoneBind,
        setDropZoneState: setDropZoneState,
        unbindAttachmentDropZone: unbindAttachmentDropZone
    };

    return AttachmentDropZoneRuntime;
});
