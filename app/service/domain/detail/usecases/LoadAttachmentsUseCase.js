sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/UseCaseValue",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (ModelContracts, Result, DetailAttachmentStateRuntime, UseCaseValue, CreateSentinel, ViewPathContracts) {
    "use strict";

    var VIEW_MODEL = ModelContracts.MODELS.VIEW;

    function LoadAttachmentsUseCase() {
        return {
            execute: function (input, ctx) { return execute(input, ctx); }
        };
    }

    function execute(mInput, mCtx) {
        var sDbKey = UseCaseValue.dbKey(mInput);
        var oRepo = mCtx && mCtx.repo;
        var oUiState = mCtx && mCtx.uiState;
        var aSessionAttachments;
        var aWorkingAttachments;

        if (!sDbKey || CreateSentinel.isCreateId(sDbKey)) {
            aSessionAttachments = (oUiState && oUiState.get(VIEW_MODEL, ViewPathContracts.SESSION_ATTACHMENTS)) || [];
            aWorkingAttachments = Array.isArray(aSessionAttachments) ? aSessionAttachments : [];
            return Promise.resolve(Result.ok({
                attachments: aWorkingAttachments
            }, DetailAttachmentStateRuntime.buildAttachmentLoadEffects(
                [],
                "",
                "info"
            )));
        }
        if (!oRepo || typeof oRepo.loadAttachments !== "function") {
            return Promise.resolve(Result.fail({
                code: "ATTACHMENT_LOAD_UNAVAILABLE"
            }, DetailAttachmentStateRuntime.buildAttachmentBusyResetEffects()));
        }

        return Promise.resolve(oRepo.loadAttachments({ dbKey: sDbKey })).then(function (oResult) {
            return Result.ok(oResult || {}, DetailAttachmentStateRuntime.buildAttachmentLoadEffects((oResult && oResult.attachments) || [], "", "info"));
        }).catch(function (oError) {
            return Result.fail(oError, DetailAttachmentStateRuntime.buildAttachmentBusyResetEffects());
        });
    }

    return LoadAttachmentsUseCase;
});
