sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/JsRuntimeContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/AttachmentValueCodec",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentIdentity",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/AttachmentEffectRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DraftChecklistFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (UseCase, Result, Effects, JsRuntimeContracts, AttachmentValueCodec, AttachmentIdentity, AttachmentEffectRuntime, DetailStateAccess, StatePaths, CreateSentinel, DraftChecklistFactory, ViewPathContracts) {
    "use strict";

    var TYPE_FUNCTION = JsRuntimeContracts.TYPEOF.FUNCTION;

    function buildLocalObjectUrl(oFile) {
        if (typeof window !== "undefined" && window.URL && typeof window.URL.createObjectURL === TYPE_FUNCTION && oFile) {
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
        return AttachmentValueCodec.fileToBase64(oFile).then(function (sFileBase64) {
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
                _file: oFile || null,
                _fileBase64: sFileBase64 || ""
            };
            return {
                attachment: oAttachment,
                attachments: aCurrent.concat([oAttachment])
            };
        });
    }

    function buildUploadEffects(mCtx, oAttachment, sToastKey) {
        var aCurrentAll = DetailStateAccess.readCurrentAttachments(mCtx);
        var oUiState = mCtx && mCtx.uiState;
        var bLoadedAll = !!(oUiState && oUiState.get("view", ViewPathContracts.ATTACHMENTS_LOADED));
        var aSession = (oUiState && oUiState.get("view", ViewPathContracts.SESSION_ATTACHMENTS)) || [];
        var aAllNext = oAttachment ? AttachmentIdentity.appendUnique(aCurrentAll, oAttachment) : (Array.isArray(aCurrentAll) ? aCurrentAll.slice() : []);
        var aSessionNext = oAttachment ? AttachmentIdentity.appendUnique(aSession, Object.assign({}, oAttachment, { _sessionUpload: true })) : (Array.isArray(aSession) ? aSession.slice() : []);
        var aEffects = AttachmentEffectRuntime.buildAttachmentSyncEffects(aAllNext, sToastKey || "attachmentUploaded", "success");
        aEffects.push(Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, aSessionNext));
        aEffects.push(Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, true));
        if (bLoadedAll) {
            aEffects.push(Effects.modelPatch("view", ViewPathContracts.ATTACHMENTS_LOADED, true));
        }
        return aEffects;
    }

    function AttachmentUploadUseCase() {
        UseCase.call(this, "AttachmentUploadUseCase");
    }

    AttachmentUploadUseCase.prototype = Object.create(UseCase.prototype);
    AttachmentUploadUseCase.prototype.constructor = AttachmentUploadUseCase;

    AttachmentUploadUseCase.prototype.execute = function (mInput, mCtx) {
        var sRootId = String((mInput && mInput.rootId) || "").trim();
        if (!sRootId && !CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(Result.fail({
                message: "Attachment target root is missing",
                code: "ATTACHMENT_TARGET_MISSING"
            }, AttachmentEffectRuntime.buildAttachmentBusyResetEffects()));
        }
        return Promise.resolve(stageLocalAttachment(mInput, mCtx)).then(function (oRes) {
            return Result.ok(
                oRes || {},
                buildUploadEffects(mCtx, (oRes && oRes.attachment) || null, "attachmentUploaded")
            );
        }).catch(function (oError) {
            return Result.fail(oError, AttachmentEffectRuntime.buildAttachmentBusyResetEffects());
        });
    };

    return AttachmentUploadUseCase;
});
