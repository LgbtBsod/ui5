sap.ui.define([
    "checklist/app/service/framework/UseCase",
    "checklist/app/service/framework/Result",
    "checklist/app/service/domain/shared/UseCaseResultUtils",
    "checklist/app/service/domain/detail/AttachmentEffectSupport",
    "checklist/app/service/domain/detail/DetailStateAccess",
    "checklist/app/util/CreateSentinel"
], function (UseCase, Result, UseCaseResultUtils, AttachmentEffectSupport, DetailStateAccess, CreateSentinel) {
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

    AttachmentDeleteUseCase.prototype = Object.create(UseCase.prototype);
    AttachmentDeleteUseCase.prototype.constructor = AttachmentDeleteUseCase;

    AttachmentDeleteUseCase.prototype.execute = function (mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sRootId = String((mInput && mInput.rootId) || "").trim();
        var oAttachment = (mInput && mInput.attachment) || null;
        if (!sRootId || CreateSentinel.isCreateId(sRootId) || (oAttachment && oAttachment.staged)) {
            cleanupObjectUrl(oAttachment);
            var aAttachments = DetailStateAccess.readCurrentAttachments(mCtx).filter(function (oItem) {
                return String((oItem && (oItem.AttachmentKey || oItem.Key)) || "").trim() !== String((mInput && mInput.attachmentId) || "").trim();
            });
            return Promise.resolve(Result.ok(
                { deleted: true, attachments: aAttachments },
                AttachmentEffectSupport.buildAttachmentSyncEffects(aAttachments, "attachmentDeleted", "info")
            ));
        }
        return UseCaseResultUtils.callOrDefault(function () {
            return oRepo && oRepo.deleteAttachment(mInput || {});
        }, { deleted: true }).then(function (oRes) {
            return Result.ok(
                oRes || {},
                AttachmentEffectSupport.buildAttachmentSyncEffects((oRes && oRes.attachments) || [], "attachmentDeleted", "info")
            );
        }).catch(function (oError) {
            return Result.fail(oError, AttachmentEffectSupport.buildAttachmentBusyResetEffects());
        });
    };

    return AttachmentDeleteUseCase;
});
