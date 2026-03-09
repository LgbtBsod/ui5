sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseResultUtils",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseInputUtils",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentEffectSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel"
], function (UseCase, Result, UseCaseResultUtils, UseCaseInputUtils, AttachmentEffectSupport, DetailStateAccess, CreateSentinel) {
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
        var sRootId = UseCaseInputUtils.rootId(mInput);
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
