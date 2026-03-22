sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (Result, JsRuntime, UseCaseValue, DetailAttachmentStateRuntime, CreateSentinel) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function cleanupObjectUrl(oAttachment) {
        var sUrl = oAttachment && oAttachment.localObjectUrl;
        if (sUrl && typeof window !== "undefined" && window.URL && typeof window.URL.revokeObjectURL === TYPE_FUNCTION) {
            window.URL.revokeObjectURL(sUrl);
        }
    }

    function AttachmentDeleteUseCase() {
        return {
            execute: execute
        };
    }

    function buildDeleteEffects(mCtx, sAttachmentId, oAttachment) {
        return DetailAttachmentStateRuntime.removeAttachment(mCtx, sAttachmentId, oAttachment, "attachmentDeleted");
    }

    function execute(mInput, mCtx) {
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
            return Result.fail(oError, DetailAttachmentStateRuntime.buildAttachmentBusyResetEffects());
        });
    }

    return AttachmentDeleteUseCase;
});
