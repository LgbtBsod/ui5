sap.ui.define([
    "sap_ui5/controller/support/AttachmentUploadSupport",
    "sap_ui5/controller/support/DetailActionConstants",
    "sap_ui5/controller/support/ControllerModelWriteSupport"
], function (AttachmentUploadSupport, DetailActionConstants, ControllerModelWriteSupport) {
    "use strict";

    function remToPx(fRem) {
        var iRootSize = parseFloat(window.getComputedStyle(document.documentElement).fontSize || "16");
        return Math.round(Number(fRem || 0) * (Number.isFinite(iRootSize) && iRootSize > 0 ? iRootSize : 16));
    }

    return {
        _scheduleAttachmentDropZoneBind: function (iAttempt) {
            var iNextAttempt = Number(iAttempt || 0);
            var oDropZone;
            if (this._iAttachmentDropZoneBindTimer) {
                clearTimeout(this._iAttachmentDropZoneBindTimer);
                this._iAttachmentDropZoneBindTimer = null;
            }
            this._iAttachmentDropZoneBindTimer = setTimeout(function () {
                var oDropZoneDom;
                this._iAttachmentDropZoneBindTimer = null;
                AttachmentUploadSupport.syncUploaderPolicy(this);
                oDropZone = this.byId("attachmentDropZone");
                oDropZoneDom = oDropZone && oDropZone.getDomRef && oDropZone.getDomRef();
                if (!oDropZoneDom) {
                    AttachmentUploadSupport.unbindDropZone(this);
                } else {
                    AttachmentUploadSupport.bindDropZone(this);
                }
                if (!oDropZoneDom && iNextAttempt < 8) {
                    this._scheduleAttachmentDropZoneBind(iNextAttempt + 1);
                }
            }.bind(this), iNextAttempt === 0 ? 0 : 180);
        },

        _setViewFlag: function (sPath, vValue) {
            ControllerModelWriteSupport.set(this, "view", sPath, vValue);
        },

        _setDeleteChecklistConfirmArmed: function (bArmed) {
            this._setViewFlag("/deleteChecklistConfirmArmed", !!bArmed);
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
            this._setViewFlag("/narrowDetailViewport", bNarrow);
            if (oView && typeof oView.toggleStyleClass === "function") {
                oView.toggleStyleClass("detailViewportNarrow", bNarrow);
            }
            this._syncViewportPinnedControlRail();
        }
    };
});
