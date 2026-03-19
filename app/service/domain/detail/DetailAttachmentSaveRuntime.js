sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentDeltaRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailAttachmentStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts"
], function (Effects, DetailAttachmentDeltaRuntime, DetailAttachmentStateRuntime, CreateSentinel, StatePaths, ViewPathContracts) {
    "use strict";

    function syncAfterSave(mOptions) {
        var oRepo = mOptions.repo;
        var sRootId = String(mOptions.rootId || "").trim();
        var bCreate = !!mOptions.createMode;
        var aCurrentAttachments = Array.isArray(mOptions.currentAttachments) ? mOptions.currentAttachments : [];
        var oSaved = mOptions.savedResult || {};
        var oCurrent = mOptions.currentChecklist || {};
        var oSavedSnapshot = mOptions.savedSnapshot || {};
        var oCtx = mOptions.ctx;
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
                Effects.modelPatch("selected", "/", oSelectedSnapshot),
                Effects.modelPatch("selected", "/attachments", aSyncedAttachments),
                Effects.modelPatch("view", ViewPathContracts.SESSION_ATTACHMENTS, aSyncedAttachments)
            ];

            if (!bHasPendingAttachments) {
                aEffects.push(Effects.modelPatch("snapshot", "/", Object.assign({}, oSavedSnapshot, {
                    attachments: aSyncedAttachments
                })));
            }

            DetailAttachmentDeltaRuntime.cleanupStagedAttachmentUrls(aCurrentAttachments);

            return {
                attachments: aSyncedAttachments,
                hasPendingAttachments: bHasPendingAttachments,
                selectedSnapshot: oSelectedSnapshot,
                snapshot: Object.assign({}, oSavedSnapshot, {
                    attachments: bHasPendingAttachments
                        ? (((mOptions.baseSnapshot || {}).attachments) || [])
                        : aSyncedAttachments
                }),
                effects: aEffects
            };
        });
    }

    return {
        syncAfterSave: syncAfterSave
    };
});
