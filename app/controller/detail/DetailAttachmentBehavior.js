sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailCommandPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentOpenRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/NavigationIntentService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime"
], function (AttachmentUploadCore, DetailCommandPolicy, DetailAttachmentRuntime, DetailAttachmentOpenRuntime, NavigationIntentService, RootIdRuntime) {
    "use strict";

    function attachmentSectionHooks(oController) {
        return {
            attachmentDelete: function (mInput) {
                return DetailCommandPolicy.attachmentDelete(oController, RootIdRuntime.withCurrentRootId(oController, mInput));
            },
            attachmentLoad: function () {
                return DetailCommandPolicy.attachmentLoad(oController, RootIdRuntime.withCurrentRootId(oController));
            },
            scheduleAttachmentDropZoneBind: function () {
                if (typeof oController._scheduleAttachmentDropZoneBind === "function") {
                    oController._scheduleAttachmentDropZoneBind();
                }
            },
            unbindAttachmentDropZone: function () {
                if (typeof oController._unbindAttachmentDropZone === "function") {
                    oController._unbindAttachmentDropZone();
                }
            }
        };
    }

    return {
        onAttachmentUploadChange: function (oEvent) {
            return AttachmentUploadCore.onUploaderChange(this, oEvent);
        },
        onDeleteAttachment: function (oEvent) {
            return DetailAttachmentRuntime.deleteAttachment(this, oEvent, attachmentSectionHooks(this));
        },
        onOpenAttachment: function (oEvent) {
            return DetailAttachmentOpenRuntime.openAttachment(this, oEvent);
        },
        onOpenAttachmentPicker: function () {
            return AttachmentUploadCore.openNativeFilePicker(this);
        },
        onOpenWorkflowAnalytics: function () {
            NavigationIntentService.navigateToAnalytics(this);
            return Promise.resolve();
        },
        onToggleAttachmentsSection: function () {
            return DetailAttachmentRuntime.toggleAttachmentsSection(this, attachmentSectionHooks(this));
        }
    };
});
