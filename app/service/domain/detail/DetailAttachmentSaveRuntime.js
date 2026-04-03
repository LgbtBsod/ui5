sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer"
], function (Effects, DetailAttachmentDeltaRuntime, ViewPathContracts, ModelContracts, DetailUseCaseConstants, ODataKeyNormalizer) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL_PATHS = DetailUseCaseConstants.MODEL_PATHS;

    function syncAfterSave(mOptions) {
        var oRepo = mOptions.repo;
        var sCurrentDbKey = String(mOptions.dbKey || mOptions.rootId || "").trim();
        var bCreate = !!mOptions.createMode;
        var aCurrentAttachments = Array.isArray(mOptions.currentAttachments) ? mOptions.currentAttachments : [];
        var oSavedSnapshot = mOptions.savedSnapshot || {};
        var sPersistedDbKey = ODataKeyNormalizer.normalizeBinaryKey(mOptions.serverRootId);
        var bNeedsAttachmentReload = bCreate;
        var sEffectiveDbKey = sPersistedDbKey || ODataKeyNormalizer.normalizeBinaryKey(sCurrentDbKey);
        return DetailAttachmentDeltaRuntime.refreshAttachments(
            oRepo,
            sEffectiveDbKey,
            bCreate ? [] : aCurrentAttachments,
            bNeedsAttachmentReload
        ).then(function (aSyncedAttachmentsRaw) {
            var aSyncedAttachments = DetailAttachmentDeltaRuntime.stripStagedAttachmentInternals(aSyncedAttachmentsRaw);
            var bHasPendingAttachments = false;
            var oSelectedSnapshot = Object.assign({}, oSavedSnapshot, { attachments: aSyncedAttachments });
            var aEffects = [
                Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ROOT, oSelectedSnapshot),
                Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.ATTACHMENTS, aSyncedAttachments),
                Effects.modelPatch(MODELS.VIEW, ViewPathContracts.SESSION_ATTACHMENTS, [])
            ];

            aEffects.push(Effects.modelPatch(MODELS.DETAIL, DETAIL_MODEL_PATHS.BASE_ATTACHMENTS, aSyncedAttachments));

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
