sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (Effects, AttachmentIdentity, DetailStateAccess, ViewPathContracts, StatePaths, ModelContracts, DetailContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;
    var DETAIL_MESSAGE_KEYS = DetailContracts;

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

    function readSessionAttachments(mCtx) {
        var oUiState = mCtx && mCtx.uiState;
        var aSession = oUiState && oUiState.get(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS);
        return Array.isArray(aSession) ? aSession : [];
    }

    function readWorkingAttachments(mCtx) {
        var aCurrentAll = DetailStateAccess.readCurrentAttachments(mCtx);
        var aSession = readSessionAttachments(mCtx);
        return aSession.length ? aSession : aCurrentAll;
    }

    function syncEffects(mCtx, aAttachments, sToastKey, sSeverity, bDirty) {
        var aPersisted = Array.isArray(aAttachments) ? aAttachments.slice() : [];
        return buildAttachmentSyncEffects(aPersisted, sToastKey || "", sSeverity || "info").concat([
            Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ATTACHMENTS, aPersisted),
            Effects.modelPatch(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS, aPersisted),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DIRTY, !!bDirty)
        ]);
    }

    function appendSessionAttachment(mCtx, oAttachment, sToastKey) {
        var aCurrentAll = readWorkingAttachments(mCtx);
        var aSession = readSessionAttachments(mCtx);
        var aAllNext = AttachmentIdentity.appendUnique(aCurrentAll, oAttachment);
        var aSessionNext = AttachmentIdentity.appendUnique(aSession, Object.assign({}, oAttachment, { _sessionUpload: true }));
        return buildAttachmentSyncEffects(aAllNext, sToastKey || DETAIL_MESSAGE_KEYS.ATTACHMENT_UPLOADED, "success").concat([
            Effects.modelPatch(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS, aSessionNext),
            Effects.modelPatch(MODELS.STATE, StatePaths.WORKFLOW_DIRTY, true)
        ]);
    }

    function removeAttachment(mCtx, sAttachmentId, oAttachment, sToastKey) {
        var aCurrentAll = readWorkingAttachments(mCtx);
        var aSession = readSessionAttachments(mCtx);
        var aAllNext = AttachmentIdentity.removeByAttachment(aCurrentAll, sAttachmentId, oAttachment);
        var aSessionNext = AttachmentIdentity.removeByAttachment(aSession, sAttachmentId, oAttachment);
        return buildAttachmentSyncEffects(aAllNext, sToastKey || DETAIL_MESSAGE_KEYS.ATTACHMENT_DELETED, "info").concat([
            Effects.modelPatch(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS, aSessionNext)
        ]);
    }

    return {
        buildAttachmentBusyResetEffects: buildAttachmentBusyResetEffects,
        buildAttachmentLoadEffects: buildAttachmentLoadEffects,
        buildAttachmentSyncEffects: buildAttachmentSyncEffects,
        appendSessionAttachment: appendSessionAttachment,
        readSessionAttachments: readSessionAttachments,
        readWorkingAttachments: readWorkingAttachments,
        removeAttachment: removeAttachment,
        syncEffects: syncEffects
    };
});
