sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/LoadAttachmentsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AttachmentUploadUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/usecases/AttachmentDeleteUseCase"
], function (
    LoadAttachmentsUseCase,
    AttachmentUploadUseCase,
    AttachmentDeleteUseCase
) {
    "use strict";

    function executeUseCase(oUseCase, mInput, mCtx) {
        return oUseCase.execute(mInput || {}, mCtx || {});
    }

    function DetailAttachmentFlowRuntime(mDeps) {
        var d = mDeps || {};
        this._uc = {
            attachmentLoad: d.attachmentLoadUseCase || LoadAttachmentsUseCase(),
            attachmentUpload: d.attachmentUploadUseCase || AttachmentUploadUseCase(),
            attachmentDelete: d.attachmentDeleteUseCase || AttachmentDeleteUseCase()
        };
    }

    DetailAttachmentFlowRuntime.prototype.attachmentLoad = function (mInput, mCtx) {
        return executeUseCase(this._uc.attachmentLoad, mInput, mCtx);
    };

    DetailAttachmentFlowRuntime.prototype.attachmentUpload = function (mInput, mCtx) {
        return executeUseCase(this._uc.attachmentUpload, mInput, mCtx);
    };

    DetailAttachmentFlowRuntime.prototype.attachmentDelete = function (mInput, mCtx) {
        return executeUseCase(this._uc.attachmentDelete, mInput, mCtx);
    };

    return DetailAttachmentFlowRuntime;
});
