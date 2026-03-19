sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentEffectRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (Effects, AttachmentIdentity, AttachmentEffectRuntime, DetailStateAccess, ViewPathContracts, StatePaths) {
    "use strict";

    function syncEffects(mCtx, aAttachments, sToastKey, sSeverity, bDirty) {
        var oUiState = mCtx && mCtx.uiState;
        var aSession = (oUiState && oUiState.get("view", ViewPathContracts.SESSION_ATTACHMENTS)) || [];
        var aPersisted = Array.isArray(aAttachments) ? aAttachments.slice() : [];
        return AttachmentEffectRuntime.buildAttachmentSyncEffects(aPersisted, sToastKey || "", sSeverity || "info").concat([
            Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, aSession),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, !!bDirty)
        ]);
    }

    function appendSessionAttachment(mCtx, oAttachment, sToastKey) {
        var oUiState = mCtx && mCtx.uiState;
        var aCurrentAll = DetailStateAccess.readCurrentAttachments(mCtx);
        var aSession = (oUiState && oUiState.get("view", ViewPathContracts.SESSION_ATTACHMENTS)) || [];
        var aAllNext = AttachmentIdentity.appendUnique(aCurrentAll, oAttachment);
        var aSessionNext = AttachmentIdentity.appendUnique(aSession, Object.assign({}, oAttachment, { _sessionUpload: true }));
        return AttachmentEffectRuntime.buildAttachmentSyncEffects(aAllNext, sToastKey || "attachmentUploaded", "success").concat([
            Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, aSessionNext),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, true)
        ]);
    }

    function removeAttachment(mCtx, sAttachmentId, oAttachment, sToastKey) {
        var oUiState = mCtx && mCtx.uiState;
        var aCurrentAll = DetailStateAccess.readCurrentAttachments(mCtx);
        var aSession = (oUiState && oUiState.get("view", ViewPathContracts.SESSION_ATTACHMENTS)) || [];
        var aAllNext = AttachmentIdentity.removeByAttachment(aCurrentAll, sAttachmentId, oAttachment);
        var aSessionNext = AttachmentIdentity.removeByAttachment(aSession, sAttachmentId, oAttachment);
        return AttachmentEffectRuntime.buildAttachmentSyncEffects(aAllNext, sToastKey || "attachmentDeleted", "info").concat([
            Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, aSessionNext)
        ]);
    }

    return {
        appendSessionAttachment: appendSessionAttachment,
        removeAttachment: removeAttachment,
        syncEffects: syncEffects
    };
});
