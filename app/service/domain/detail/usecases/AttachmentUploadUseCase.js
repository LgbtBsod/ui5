sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseResultUtils",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentEffectSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/util/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/util/DraftChecklistFactory"
], function (UseCase, Result, UseCaseResultUtils, AttachmentEffectSupport, DetailStateAccess, CreateSentinel, DraftChecklistFactory) {
    "use strict";

    function buildLocalObjectUrl(oFile) {
        if (typeof window !== "undefined" && window.URL && typeof window.URL.createObjectURL === "function" && oFile) {
            return window.URL.createObjectURL(oFile);
        }
        return "";
    }

    function stageLocalAttachment(mInput, mCtx) {
        var oSnapshot = DetailStateAccess.readCurrentChecklist(mCtx);
        var aCurrent = (oSnapshot && oSnapshot.attachments) || [];
        var oFile = mInput && mInput.file;
        var oMeta = (mInput && mInput.fileMeta) || {};
        var sRootKey = String((((oSnapshot || {}).root || {}).id) || (((oSnapshot || {}).root || {}).Key) || "").trim() || DraftChecklistFactory.createTempKey();
        var sAttachmentKey = DraftChecklistFactory.createTempKey();
        var oAttachment = {
            AttachmentKey: sAttachmentKey,
            Key: sAttachmentKey,
            client_row_id: sAttachmentKey,
            RootKey: sRootKey,
            FolderKey: sRootKey,
            FileName: oMeta.fileName || (oFile && oFile.name) || "",
            fileName: oMeta.fileName || (oFile && oFile.name) || "",
            MimeType: oMeta.mimeType || (oFile && oFile.type) || "application/octet-stream",
            mimeType: oMeta.mimeType || (oFile && oFile.type) || "application/octet-stream",
            FileSize: Number(oMeta.fileSize || (oFile && oFile.size) || 0) || 0,
            fileSize: Number(oMeta.fileSize || (oFile && oFile.size) || 0) || 0,
            CategoryKey: String(oMeta.categoryKey || "GEN").trim() || "GEN",
            categoryKey: String(oMeta.categoryKey || "GEN").trim() || "GEN",
            CreatedOn: new Date().toISOString(),
            ChangedOn: new Date().toISOString(),
            staged: true,
            localObjectUrl: buildLocalObjectUrl(oFile),
            _file: oFile || null
        };
        return {
            attachment: oAttachment,
            attachments: aCurrent.concat([oAttachment])
        };
    }

    function AttachmentUploadUseCase() {
        UseCase.call(this, "AttachmentUploadUseCase");
    }

    AttachmentUploadUseCase.prototype = Object.create(UseCase.prototype);
    AttachmentUploadUseCase.prototype.constructor = AttachmentUploadUseCase;

    AttachmentUploadUseCase.prototype.execute = function (mInput, mCtx) {
        var oRepo = mCtx && mCtx.repo;
        var sRootId = String((mInput && mInput.rootId) || "").trim();
        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(stageLocalAttachment(mInput, mCtx)).then(function (oRes) {
                return Result.ok(
                    oRes || {},
                    AttachmentEffectSupport.buildAttachmentSyncEffects((oRes && oRes.attachments) || [], "attachmentUploaded", "success")
                );
            });
        }
        return UseCaseResultUtils.callOrDefault(function () {
            return oRepo && oRepo.uploadAttachment(mInput || {});
        }, { attachment: {} }).then(function (oRes) {
            return Result.ok(
                oRes || {},
                AttachmentEffectSupport.buildAttachmentSyncEffects((oRes && oRes.attachments) || [], "attachmentUploaded", "success")
            );
        }).catch(function (oError) {
            return Result.fail(oError, AttachmentEffectSupport.buildAttachmentBusyResetEffects());
        });
    };

    return AttachmentUploadUseCase;
});
