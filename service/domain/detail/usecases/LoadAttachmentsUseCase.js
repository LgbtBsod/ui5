sap.ui.define([
    "sap_ui5/service/framework/UseCase",
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/domain/detail/AttachmentEffectSupport",
    "sap_ui5/util/CreateSentinel"
], function (UseCase, Result, AttachmentEffectSupport, CreateSentinel) {
    "use strict";

    function LoadAttachmentsUseCase() {
        UseCase.call(this, "LoadAttachmentsUseCase");
    }

    LoadAttachmentsUseCase.prototype = Object.create(UseCase.prototype);
    LoadAttachmentsUseCase.prototype.constructor = LoadAttachmentsUseCase;

    LoadAttachmentsUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = String((mInput && mInput.rootId) || "").trim();
        var oRepo = mCtx && mCtx.repo;

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(Result.ok({
                attachments: []
            }, AttachmentEffectSupport.buildAttachmentLoadEffects([], "", "info")));
        }
        if (!oRepo || typeof oRepo.loadAttachments !== "function") {
            return Promise.resolve(Result.fail({
                message: "Attachment loader unavailable",
                code: "ATTACHMENT_LOAD_UNAVAILABLE"
            }, AttachmentEffectSupport.buildAttachmentBusyResetEffects()));
        }

        return Promise.resolve(oRepo.loadAttachments({ rootId: sRootId })).then(function (oResult) {
            return Result.ok(oResult || {}, AttachmentEffectSupport.buildAttachmentLoadEffects((oResult && oResult.attachments) || [], "", "info"));
        }).catch(function (oError) {
            return Result.fail(oError, AttachmentEffectSupport.buildAttachmentBusyResetEffects());
        });
    };

    return LoadAttachmentsUseCase;
});