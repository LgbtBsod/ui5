sap.ui.define([
    "sap_ui5/service/framework/Effects"
], function (Effects) {
    "use strict";

    function buildAttachmentBusyResetEffects() {
        return [Effects.modelPatch("view", "/attachmentBusy", false)];
    }

    function buildAttachmentSyncEffects(aAttachments, sToastKey, sToastLevel) {
        var aSafeAttachments = Array.isArray(aAttachments) ? aAttachments : [];
        return [
            Effects.modelPatch("selected", "/attachments", aSafeAttachments),
            Effects.modelPatch("uiState", "/_detailCurrent/attachments", aSafeAttachments),
            Effects.modelPatch("uiState", "/_detailSnapshot/attachments", aSafeAttachments)
        ].concat(buildAttachmentBusyResetEffects(), [
            Effects.toast(sToastKey || "attachmentUpdated", sToastLevel || "info")
        ]);
    }

    return {
        buildAttachmentBusyResetEffects: buildAttachmentBusyResetEffects,
        buildAttachmentSyncEffects: buildAttachmentSyncEffects
    };
});
