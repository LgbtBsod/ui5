sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentDropZoneRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailAdaptiveViewportRuntime"
], function (AttachmentDropZoneRuntime, DetailAdaptiveViewportRuntime) {
    "use strict";

    return {
        _bindAttachmentDropZone: function () {
            AttachmentDropZoneRuntime.bindAttachmentDropZone(this);
        },

        _unbindAttachmentDropZone: function () {
            AttachmentDropZoneRuntime.unbindAttachmentDropZone(this);
        },

        _scheduleAttachmentDropZoneBind: function (iAttempt) {
            AttachmentDropZoneRuntime.scheduleAttachmentDropZoneBind(this, iAttempt);
        },

        _bindAdaptiveDetailViewport: function () {
            DetailAdaptiveViewportRuntime.bindAdaptiveDetailViewport(this);
        },

        _unbindAdaptiveDetailViewport: function () {
            DetailAdaptiveViewportRuntime.unbindAdaptiveDetailViewport(this);
        },

        _syncAdaptiveDetailViewport: function () {
            DetailAdaptiveViewportRuntime.syncAdaptiveDetailViewport(this);
        }
    };
});
