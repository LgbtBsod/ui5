sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailStateAccess",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (ModelContracts, Result, DetailAttachmentStateRuntime, UseCaseValue, CreateSentinel, DetailStateAccess, ViewPathContracts) {
    "use strict";

    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

    function LoadAttachmentsUseCase() {
        return {
            execute: execute
        };
    }

    function execute(mInput, mCtx) {
        var sRootId = UseCaseValue.rootId(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oUiState = mCtx && mCtx.uiState;
        var aCurrentAttachments;
        var aSessionAttachments;
        var aWorkingAttachments;

        if (!sRootId || CreateSentinel.isCreateId(sRootId)) {
            aCurrentAttachments = DetailStateAccess.readCurrentAttachments(mCtx);
            aSessionAttachments = (oUiState && oUiState.get(VIEW_MODEL, ViewPathContracts.SESSION_ATTACHMENTS)) || [];
            aWorkingAttachments = aSessionAttachments.length ? aSessionAttachments : aCurrentAttachments;
            return Promise.resolve(Result.ok({
                attachments: aWorkingAttachments
            }, DetailAttachmentStateRuntime.buildAttachmentLoadEffects(
                aWorkingAttachments,
                "",
                "info"
            )));
        }
        if (!oRepo || typeof oRepo.loadAttachments !== "function") {
            return Promise.resolve(Result.fail({
                message: "Attachment loader unavailable",
                code: "ATTACHMENT_LOAD_UNAVAILABLE"
            }, DetailAttachmentStateRuntime.buildAttachmentBusyResetEffects()));
        }

        return Promise.resolve(oRepo.loadAttachments({ rootId: sRootId })).then(function (oResult) {
            return Result.ok(oResult || {}, DetailAttachmentStateRuntime.buildAttachmentLoadEffects((oResult && oResult.attachments) || [], "", "info"));
        }).catch(function (oError) {
            return Result.fail(oError, DetailAttachmentStateRuntime.buildAttachmentBusyResetEffects());
        });
    }

    return LoadAttachmentsUseCase;
});
