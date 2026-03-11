sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseResultUtils",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseInputUtils",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentEffectSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (UseCase, Result, Effects, UseCaseResultUtils, UseCaseInputUtils, AttachmentIdentity, AttachmentEffectSupport, DetailStateAccess, CreateSentinel) {
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

    function buildDeleteEffects(mCtx, sAttachmentId) {
        var oUiState = mCtx && mCtx.uiState;
        var aCurrentAll = DetailStateAccess.readCurrentAttachments(mCtx);
        var aSession = (oUiState && oUiState.get("view", "/sessionAttachments")) || [];
        var aAllNext = AttachmentIdentity.removeById(aCurrentAll, sAttachmentId);
        var aSessionNext = AttachmentIdentity.removeById(aSession, sAttachmentId);
        var aEffects = AttachmentEffectSupport.buildAttachmentSyncEffects(aAllNext, "attachmentDeleted", "info");
        aEffects.push(Effects.modelPatch("view", "/sessionAttachments", aSessionNext));
        return aEffects;
    }

    AttachmentDeleteUseCase.prototype = Object.create(UseCase.prototype);
    AttachmentDeleteUseCase.prototype.constructor = AttachmentDeleteUseCase;

    AttachmentDeleteUseCase.prototype.execute = function (mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sRootId = UseCaseInputUtils.rootId(mInput);
        var oAttachment = (mInput && mInput.attachment) || null;
        var sAttachmentId = String((mInput && mInput.attachmentId) || "").trim();
        if (!sRootId || CreateSentinel.isCreateId(sRootId) || (oAttachment && oAttachment.staged)) {
            cleanupObjectUrl(oAttachment);
            return Promise.resolve(Result.ok(
                { deleted: true },
                buildDeleteEffects(mCtx, sAttachmentId)
            ));
        }
        return UseCaseResultUtils.callOrDefault(function () {
            return oRepo && oRepo.deleteAttachment(mInput || {});
        }, { deleted: true }).then(function (oRes) {
            return Result.ok(
                oRes || {},
                buildDeleteEffects(mCtx, sAttachmentId)
            );
        }).catch(function (oError) {
            return Result.fail(oError, AttachmentEffectSupport.buildAttachmentBusyResetEffects());
        });
    };

    return AttachmentDeleteUseCase;
});
