sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentEffectRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (UseCase, Result, Effects, UseCaseValue, AttachmentIdentity, AttachmentEffectRuntime, DetailStateAccess, CreateSentinel, ViewPathContracts) {
    "use strict";

    function cleanupObjectUrl(oAttachment) {
        var sUrl = oAttachment && oAttachment.localObjectUrl;
        if (sUrl && typeof window !== "undefined" && window.URL && typeof window.URL.revokeObjectURL === "function") {
            window.URL.revokeObjectURL(sUrl);
        }
    }

    function AttachmentDeleteUseCase() {
        UseCase.call(this, "AttachmentDeleteUseCase");
    }

    function buildDeleteEffects(mCtx, sAttachmentId, oAttachment) {
        var oUiState = mCtx && mCtx.uiState;
        var aCurrentAll = DetailStateAccess.readCurrentAttachments(mCtx);
        var aSession = (oUiState && oUiState.get("view", ViewPathContracts.SESSION_ATTACHMENTS)) || [];
        var aAllNext = AttachmentIdentity.removeByAttachment(aCurrentAll, sAttachmentId, oAttachment);
        var aSessionNext = AttachmentIdentity.removeByAttachment(aSession, sAttachmentId, oAttachment);
        var aEffects = AttachmentEffectRuntime.buildAttachmentSyncEffects(aAllNext, "attachmentDeleted", "info");
        aEffects.push(Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, aSessionNext));
        return aEffects;
    }

    AttachmentDeleteUseCase.prototype = Object.create(UseCase.prototype);
    AttachmentDeleteUseCase.prototype.constructor = AttachmentDeleteUseCase;

    AttachmentDeleteUseCase.prototype.execute = function (mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sRootId = UseCaseValue.rootId(mInput);
        var oAttachment = (mInput && mInput.attachment) || null;
        var sAttachmentId = String((mInput && mInput.attachmentId) || "").trim();
        if (!sRootId || CreateSentinel.isCreateId(sRootId) || (oAttachment && oAttachment.staged)) {
            cleanupObjectUrl(oAttachment);
            return Promise.resolve(Result.ok(
                { deleted: true },
                buildDeleteEffects(mCtx, sAttachmentId, oAttachment)
            ));
        }
        return UseCaseValue.callOrDefault(function () {
            return oRepo && oRepo.deleteAttachment(mInput || {});
        }, { deleted: true }).then(function (oRes) {
            return Result.ok(
                oRes || {},
                buildDeleteEffects(mCtx, sAttachmentId, oAttachment)
            );
        }).catch(function (oError) {
            return Result.fail(oError, AttachmentEffectRuntime.buildAttachmentBusyResetEffects());
        });
    };

    return AttachmentDeleteUseCase;
});
