sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (Effects, DetailAttachmentDeltaRuntime, DetailAttachmentStateRuntime, CreateSentinel, StatePaths, ViewPathContracts, ModelContracts, DetailUseCaseConstants) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function syncAfterSave(mOptions) {
        var oRepo = mOptions.repo;
        var sRootId = String(mOptions.rootId || "").trim();
        var bCreate = !!mOptions.createMode;
        var aCurrentAttachments = Array.isArray(mOptions.currentAttachments) ? mOptions.currentAttachments : [];
        var oSavedSnapshot = mOptions.savedSnapshot || {};
        var sServerRootId = String(mOptions.serverRootId || "").trim();
        var bNeedsAttachmentReload = bCreate || !!mOptions.hasStagedPayload;

        return DetailAttachmentDeltaRuntime.refreshAttachments(
            oRepo,
            sServerRootId || sRootId,
            aCurrentAttachments,
            bNeedsAttachmentReload
        ).then(function (aSyncedAttachmentsRaw) {
            var aSyncedAttachments = DetailAttachmentDeltaRuntime.stripStagedAttachmentInternals(aSyncedAttachmentsRaw);
            var bHasPendingAttachments = DetailAttachmentDeltaRuntime.hasPendingStagedAttachments(aCurrentAttachments);
            var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aSyncedAttachments });
            var aEffects = [
                Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ROOT, oSelectedSnapshot),
                Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ATTACHMENTS, aSyncedAttachments),
                Effects.modelPatch(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS, aSyncedAttachments)
            ];

            if (!bHasPendingAttachments) {
                aEffects.push(Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.BASE_ATTACHMENTS, aSyncedAttachments));
            }

            DetailAttachmentDeltaRuntime.cleanupStagedAttachmentUrls(aCurrentAttachments);

            return {
                attachments: aSyncedAttachments,
                hasPendingAttachments: bHasPendingAttachments,
                selectedSnapshot: oSelectedSnapshot,
                snapshot: Object.assign({}, oSavedSnapshot, {
                    attachments: []
                }),
                effects: aEffects
            };
        });
    }

    return {
        syncAfterSave: syncAfterSave
    };
});
