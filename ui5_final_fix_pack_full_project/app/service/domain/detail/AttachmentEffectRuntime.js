sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (Effects, ViewPathContracts) {
    "use strict";

    function buildAttachmentBusyResetEffects() {
        return [Effects.modelPatch("view", "/attachmentBusy", false)];
    }

    function buildAttachmentLoadEffects(aAttachments, sToastKey, sToastLevel) {
        var aSafeAttachments = Array.isArray(aAttachments) ? aAttachments : [];
        var aEffects = [
            Effects.modelPatch("selected", "/attachments", aSafeAttachments),
            Effects.modelPatch("view", ViewPathContracts.ATTACHMENTS_LOADED, true)
        ].concat(buildAttachmentBusyResetEffects());
        if (sToastKey) {
            aEffects.push(Effects.toast(sToastKey, sToastLevel || "info"));
        }
        return aEffects;
    }

    function buildAttachmentSyncEffects(aAttachments, sToastKey, sToastLevel) {
        var aSafeAttachments = Array.isArray(aAttachments) ? aAttachments : [];
        var aEffects = [
            Effects.modelPatch("selected", "/attachments", aSafeAttachments)
        ].concat(buildAttachmentBusyResetEffects());
        if (sToastKey) {
            aEffects.push(Effects.toast(sToastKey, sToastLevel || "info"));
        }
        return aEffects;
    }

    return {
        buildAttachmentBusyResetEffects: buildAttachmentBusyResetEffects,
        buildAttachmentLoadEffects: buildAttachmentLoadEffects,
        buildAttachmentSyncEffects: buildAttachmentSyncEffects
    };
});
