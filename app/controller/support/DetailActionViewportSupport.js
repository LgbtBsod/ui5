sap.ui.define([
    "checklist/app/controller/support/AttachmentUploadCore",
    "checklist/app/controller/support/AttachmentDropZoneRuntime",
    "checklist/app/controller/support/DetailActionConstants",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/SchedulingRuntime"
], function (AttachmentUploadCore, AttachmentDropZoneRuntime, DetailActionConstants, ControllerViewStateRuntime, SchedulingRuntime) {
    "use strict";

    function remToPx(fRem) {
        var iRootSize = parseFloat(window.getComputedStyle(document.documentElement).fontSize || "16");
        return Math.round(Number(fRem || 0) * (Number.isFinite(iRootSize) && iRootSize > 0 ? iRootSize : 16));
    }

    function ensureDropZoneDelegate(oController, oDropZone) {
        if (!oDropZone || !oDropZone.addEventDelegate || oController._attachmentDropZoneDelegate) {
            return;
        }
        oController._attachmentDropZoneDelegate = {
            onAfterRendering: function () {
                oController._bindAttachmentDropZone();
            }
        };
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

    return {
        _bindAttachmentDropZone: function () {
            var oDropZone = this.byId("attachmentDropZone");
            var oDomRef = oDropZone && oDropZone.getDomRef && oDropZone.getDomRef();

            AttachmentUploadCore.syncUploaderPolicy(this);
            ensureDropZoneDelegate(this, oDropZone);
            if (!oDomRef) {
                this._unbindAttachmentDropZone();
                return;
            }
            if (this._attachmentDropZoneDom === oDomRef) {
                return;
            }

            this._unbindAttachmentDropZone();
            this._attachmentDragDepth = 0;
            ensureHandlers(this, AttachmentDropZoneRuntime.dropScopeSpecs);
            ensureHandlers(this, AttachmentDropZoneRuntime.globalSpecs);
            toggleListeners(oDomRef, AttachmentDropZoneRuntime.dropScopeSpecs, this, true);
            toggleListeners(document, AttachmentDropZoneRuntime.globalSpecs, this, true);
            this._attachmentDropZoneDom = oDomRef;
            this._attachmentDropScopeDom = oDomRef;
        },

        _unbindAttachmentDropZone: function () {
            var oDropZone = this.byId && this.byId("attachmentDropZone");
            if (!this._attachmentDropZoneDom) {
                clearDropZoneDelegate(this, oDropZone);
                return;
            }
            if (this._attachmentDropScopeDom) {
                toggleListeners(this._attachmentDropScopeDom, AttachmentDropZoneRuntime.dropScopeSpecs, this, false);
            }
            toggleListeners(document, AttachmentDropZoneRuntime.globalSpecs, this, false);
            this._attachmentDropZoneDom = null;
            this._attachmentDropScopeDom = null;
            this._attachmentDragDepth = 0;
            this._attachmentGlobalDragDepth = 0;
            AttachmentDropZoneRuntime.resetVisual(this);
            clearDropZoneDelegate(this, oDropZone);
        },

        _scheduleAttachmentDropZoneBind: function (iAttempt) {
            var iNextAttempt = Number(iAttempt || 0);
            var oDropZone;
            this._iAttachmentDropZoneBindTimer = SchedulingRuntime.restartTimer(this._iAttachmentDropZoneBindTimer, function () {
                var oDropZoneDom;
                this._iAttachmentDropZoneBindTimer = null;
                AttachmentUploadCore.syncUploaderPolicy(this);
                oDropZone = this.byId("attachmentDropZone");
                oDropZoneDom = oDropZone && oDropZone.getDomRef && oDropZone.getDomRef();
                if (!oDropZoneDom) {
                    this._unbindAttachmentDropZone();
                } else {
                    this._bindAttachmentDropZone();
                }
                if (!oDropZoneDom && iNextAttempt < 8) {
                    this._scheduleAttachmentDropZoneBind(iNextAttempt + 1);
                }
            }.bind(this), iNextAttempt === 0 ? 0 : 180);
        },

        _bindAdaptiveDetailViewport: function () {
            var oView = this.getView && this.getView();
            var oDom = oView && oView.getDomRef && oView.getDomRef();
            if (!oDom) {
                return;
            }
            if (!this._fnAdaptiveViewportSync) {
                this._fnAdaptiveViewportSync = this._syncAdaptiveDetailViewport.bind(this);
            }
            this._unbindAdaptiveDetailViewport();
            if (typeof ResizeObserver === "function") {
                this._oAdaptiveViewportResizeObserver = new ResizeObserver(this._fnAdaptiveViewportSync);
                this._oAdaptiveViewportResizeObserver.observe(oDom);
            }
            window.addEventListener("resize", this._fnAdaptiveViewportSync, true);
            this._syncAdaptiveDetailViewport();
        },

        _unbindAdaptiveDetailViewport: function () {
            if (this._oAdaptiveViewportResizeObserver && typeof this._oAdaptiveViewportResizeObserver.disconnect === "function") {
                this._oAdaptiveViewportResizeObserver.disconnect();
            }
            if (this._fnAdaptiveViewportSync) {
                window.removeEventListener("resize", this._fnAdaptiveViewportSync, true);
            }
            this._oAdaptiveViewportResizeObserver = null;
        },

        _syncAdaptiveDetailViewport: function () {
            var oView = this.getView && this.getView();
            var oObjectPage = this.byId("detailObjectPage");
            var oDom = (oObjectPage && oObjectPage.getDomRef && oObjectPage.getDomRef()) || (oView && oView.getDomRef && oView.getDomRef());
            var iWidth;
            var bNarrow;
            if (!oDom) {
                return;
            }
            iWidth = Math.round((oDom.getBoundingClientRect && oDom.getBoundingClientRect().width) || 0);
            bNarrow = iWidth > 0 && iWidth <= remToPx(DetailActionConstants.DETAIL_NARROW_VIEWPORT_REM);
            ControllerViewStateRuntime.setFlag(this, "/narrowDetailViewport", bNarrow);
            if (oView && typeof oView.toggleStyleClass === "function") {
                oView.toggleStyleClass("detailViewportNarrow", bNarrow);
            }
            this._syncViewportPinnedControlRail();
        }
    };
});
