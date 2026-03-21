sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailUseCaseConstants"
], function (Effects, ViewPathContracts, ModelContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function buildAttachmentBusyResetEffects() {
        return [Effects.modelPatch(MODELS.VIEW, "/attachmentBusy", false)];
    }

    function buildAttachmentLoadEffects(aAttachments, sToastKey, sToastLevel) {
        var aSafeAttachments = Array.isArray(aAttachments) ? aAttachments : [];
        var aEffects = [
            Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ATTACHMENTS, aSafeAttachments),
            Effects.modelPatch(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS, aSafeAttachments),
            Effects.modelPatch(MODELS.VIEW, ViewPathContracts.ATTACHMENTS_LOADED, true)
        ].concat(buildAttachmentBusyResetEffects());
        if (sToastKey) {
            aEffects.push(Effects.toast(sToastKey, sToastLevel || "info"));
        }
        return aEffects;
    }

    function buildAttachmentSyncEffects(aAttachments, sToastKey, sToastLevel) {
        var aEffects = buildAttachmentBusyResetEffects();
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
