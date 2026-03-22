sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/AttachmentValueCodec",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DraftChecklistFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (ModelContracts, Result, Effects, JsRuntime, DetailAttachmentStateRuntime, AttachmentValueCodec, DetailStateAccess, CreateSentinel, DraftChecklistFactory, ViewPathContracts) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

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
        var oUiState = mCtx && mCtx.uiState;
        var bLoadedAll = !!(oUiState && oUiState.get(VIEW_MODEL, ViewPathContracts.ATTACHMENTS_LOADED));
        var aEffects = DetailAttachmentStateRuntime.appendSessionAttachment(mCtx, oAttachment, sToastKey);
        if (bLoadedAll) {
            aEffects.push(Effects.modelPatch(VIEW_MODEL, ViewPathContracts.ATTACHMENTS_LOADED, true));
        }
        return aEffects;
    }

    function AttachmentUploadUseCase() {
        return {
            execute: execute
        };
    }

    function execute(mInput, mCtx) {
        var sRootId = String((mInput && mInput.rootId) || "").trim();
        if (!sRootId && !CreateSentinel.isCreateId(sRootId)) {
            return Promise.resolve(Result.fail({
                message: "Attachment target root is missing",
                code: "ATTACHMENT_TARGET_MISSING"
            }, DetailAttachmentStateRuntime.buildAttachmentBusyResetEffects()));
        }
        return Promise.resolve(stageLocalAttachment(mInput, mCtx)).then(function (oRes) {
            return Result.ok(
                oRes || {},
                buildUploadEffects(mCtx, (oRes && oRes.attachment) || null, "attachmentUploaded")
            );
        }).catch(function (oError) {
            return Result.fail(oError, DetailAttachmentStateRuntime.buildAttachmentBusyResetEffects());
        });
    }

    return AttachmentUploadUseCase;
});
