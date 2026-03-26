sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer"
], function (Effects, DetailAttachmentDeltaRuntime, DetailAttachmentStateRuntime, CreateSentinel, StatePaths, ViewPathContracts, ModelContracts, DetailUseCaseConstants, ODataKeyNormalizer) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function uploadPendingAttachments(oRepo, sRootId, aCurrentAttachments, sSessionGuid) {
        var aPendingUploads = DetailAttachmentDeltaRuntime.listPendingStagedAttachments(aCurrentAttachments, sRootId);
        if (!sRootId || !aPendingUploads.length || !oRepo || typeof oRepo.uploadAttachment !== "function") {
            return Promise.resolve([]);
        }
        return Promise.all(aPendingUploads.map(function (oPending) {
            return Promise.resolve(oRepo.uploadAttachment({
                dbKey: sRootId,
                sessionGuid: sSessionGuid,
                attachment: oPending
            })).catch(function () {
                return null;
            });
        }));
    }

    function syncAfterSave(mOptions) {
        var oRepo = mOptions.repo;
        var sRootId = String(mOptions.rootId || "").trim();
        var bCreate = !!mOptions.createMode;
        var aCurrentAttachments = Array.isArray(mOptions.currentAttachments) ? mOptions.currentAttachments : [];
        var oSavedSnapshot = mOptions.savedSnapshot || {};
        var sServerRootId = ODataKeyNormalizer.normalizeBinaryKey(mOptions.serverRootId);
        var bNeedsAttachmentReload = bCreate || !!mOptions.hasStagedPayload;
        var sEffectiveRootId = sServerRootId || ODataKeyNormalizer.normalizeBinaryKey(sRootId);

        return uploadPendingAttachments(oRepo, sEffectiveRootId, aCurrentAttachments, mOptions.sessionGuid).then(function () {
            return DetailAttachmentDeltaRuntime.refreshAttachments(
                oRepo,
                sEffectiveRootId,
                aCurrentAttachments,
                bNeedsAttachmentReload || DetailAttachmentDeltaRuntime.hasPendingStagedAttachments(aCurrentAttachments)
            );
        }).then(function (aSyncedAttachmentsRaw) {
            var aSyncedAttachments = DetailAttachmentDeltaRuntime.stripStagedAttachmentInternals(aSyncedAttachmentsRaw);
            var bHasPendingAttachments = DetailAttachmentDeltaRuntime.hasPendingStagedAttachments(aCurrentAttachments);
            var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aSyncedAttachments });
            var aEffects = [
                Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ROOT, oSelectedSnapshot),
                Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ATTACHMENTS, aSyncedAttachments),
                Effects.modelPatch(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS, [])
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
