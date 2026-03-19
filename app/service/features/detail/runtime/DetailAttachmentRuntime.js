sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFieldContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (ModelStateRuntime, ModelContracts, DetailFieldContracts, DetailAttachmentViewState, CreateSentinel) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var VIEW_MODEL = MODELS.VIEW;
    var SELECTED_MODEL = MODELS.SELECTED;
    var VIEW_PATHS = DetailFieldContracts.VIEW_PATHS;

    function resolveAttachmentContext(oEvent) {
        var oSource = oEvent && oEvent.getSource && oEvent.getSource();
        return (oSource && oSource.getBindingContext && (oSource.getBindingContext(SELECTED_MODEL) || oSource.getBindingContext(VIEW_MODEL))) || null;
    }

    function deleteAttachment(oController, oEvent, mHooks) {
        var oCtx = resolveAttachmentContext(oEvent);
        var oRow = oCtx && oCtx.getObject && oCtx.getObject();
        var sAttachmentId = String((oRow && (oRow.AttachmentKey || oRow.Key)) || "").trim();
        if (!sAttachmentId) {
            return Promise.resolve(false);
        }
        return ModelStateRuntime.withFlag(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENT_BUSY, function () {
            return mHooks.attachmentDelete({
                attachmentId: sAttachmentId,
                attachment: oRow || null
            });
        });
    }

    function toggleAttachmentsSection(oController, mHooks) {
        var bExpanded = !!ModelStateRuntime.read(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_EXPANDED, false);
        var bLoaded = !!ModelStateRuntime.read(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_LOADED, false);
        var aSessionAttachments = ModelStateRuntime.read(oController, VIEW_MODEL, VIEW_PATHS.SESSION_ATTACHMENTS, []) || [];
        var sRootId = String((oController && oController._currentRootId && oController._currentRootId()) || "").trim();
        if (bExpanded) {
            ModelStateRuntime.write(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_EXPANDED, false);
            DetailAttachmentViewState.sync(oController);
            mHooks.unbindAttachmentDropZone();
            return Promise.resolve({ collapsed: true });
        }
        ModelStateRuntime.write(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_EXPANDED, true);
        DetailAttachmentViewState.sync(oController);
        mHooks.scheduleAttachmentDropZoneBind();
        if (CreateSentinel.isCreateId(sRootId)) {
            ModelStateRuntime.write(oController, SELECTED_MODEL, "/attachments", aSessionAttachments);
            ModelStateRuntime.write(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENTS_LOADED, true);
            return Promise.resolve({ expanded: true, staged: true });
        }
        if (bLoaded) {
            return Promise.resolve({ expanded: true, loaded: true });
        }
        return ModelStateRuntime.withFlag(oController, VIEW_MODEL, VIEW_PATHS.ATTACHMENT_BUSY, function () {
            return mHooks.attachmentLoad();
        });
    }

    return {
        deleteAttachment: deleteAttachment,
        toggleAttachmentsSection: toggleAttachmentsSection
    };
});
