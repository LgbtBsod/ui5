sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentDropZoneBindingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailAdaptiveViewportRuntime"
], function (AttachmentDropZoneBindingRuntime, DetailAdaptiveViewportRuntime) {
    "use strict";

    return {
        _bindAttachmentDropZone: function () {
            AttachmentDropZoneBindingRuntime.bindAttachmentDropZone(this);
        },

        _unbindAttachmentDropZone: function () {
            AttachmentDropZoneBindingRuntime.unbindAttachmentDropZone(this);
        },

        _scheduleAttachmentDropZoneBind: function (iAttempt) {
            AttachmentDropZoneBindingRuntime.scheduleAttachmentDropZoneBind(this, iAttempt);
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
