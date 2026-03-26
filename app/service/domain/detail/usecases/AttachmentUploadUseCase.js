sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DraftChecklistFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (ModelContracts, MessageCodeConstants, MessageKeyConstants, Result, Effects, JsRuntime, DetailAttachmentStateRuntime, DetailStateAccess, CreateSentinel, DraftChecklistFactory, ViewPathContracts) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var VIEW_MODEL = ModelContracts.MODELS.VIEW;
    var DETAIL_MESSAGE_CODES = MessageCodeConstants.DETAIL;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;

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
        var sParentKey = String((((oSnapshot || {}).root || {}).id) || (((oSnapshot || {}).root || {}).DB_KEY) || "").trim() || DraftChecklistFactory.createTempKey();
        var sAttachmentKey = DraftChecklistFactory.createTempKey();
        var oAttachment = {
            AttachmentKey: sAttachmentKey,
            Key: sAttachmentKey,
            client_row_id: sAttachmentKey,
            DB_KEY: sAttachmentKey,
            PARENT_KEY: sParentKey,
            FolderKey: sParentKey,
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
            uploadState: "PENDING_UPLOAD",
            localObjectUrl: buildLocalObjectUrl(oFile),
            _file: oFile || null
        };
        return Promise.resolve({
            attachment: oAttachment,
            attachments: aCurrent.concat([oAttachment])
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
        var sDbKey = String((mInput && (mInput.dbKey || mInput.rootId)) || "").trim();
        if (!sDbKey && !CreateSentinel.isCreateId(sDbKey)) {
            return Promise.resolve(Result.fail({
                code: DETAIL_MESSAGE_CODES.INVALID_INPUT,
                messageKey: DETAIL_MESSAGE_KEYS.ATTACHMENT_TARGET_MISSING
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
