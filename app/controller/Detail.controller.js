sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/DetailControllerLifecycle",
    "checklist/app/controller/support/DetailFormatters",
    "checklist/app/controller/support/AttachmentUploadSupport",
    "checklist/app/controller/support/DetailControllerActions"
], function (
    BaseController,
    DetailControllerLifecycle,
    DetailFormatters,
    AttachmentUploadSupport,
    DetailControllerActions
) {
    "use strict";

    var mControllerDefinition = Object.assign({}, DetailControllerLifecycle, DetailControllerActions, {
        formatValidationState: DetailFormatters.formatValidationState,
        formatValidationText: DetailFormatters.formatValidationText,
        formatValidationSummaryText: DetailFormatters.formatValidationSummaryText,
        formatValidationSummaryLinkText: DetailFormatters.formatValidationSummaryLinkText,
        formatBooleanResultText: DetailFormatters.formatBooleanResultText,
        formatBooleanResultState: DetailFormatters.formatBooleanResultState,
        formatLifecycleStatusText: DetailFormatters.formatLifecycleStatusText,
        formatLifecycleStatusState: DetailFormatters.formatLifecycleStatusState,
        formatDraftStateText: DetailFormatters.formatDraftStateText,
        formatDraftStateState: DetailFormatters.formatDraftStateState,
        formatLockOperationText: DetailFormatters.formatLockOperationText,
        formatLockOperationState: DetailFormatters.formatLockOperationState,
        formatPassedTotal: DetailFormatters.formatPassedTotal,
        formatAttachmentCategoryText: DetailFormatters.formatAttachmentCategoryText,
        hasRows: function (a) { return Array.isArray(a) && a.length > 0; },
        isEmptyRows: function (a) { return !Array.isArray(a) || a.length === 0; },
        isDesktopTableVisible: function (bPhone, bNarrow, aRows) { return !bPhone && !bNarrow && this.hasRows(aRows); },
        isPhoneTableVisible: function (bPhone, bNarrow, aRows) { return !!(bPhone || bNarrow) && this.hasRows(aRows); },
        isBarriersRatioVisible: function (sLpcKey, aRows) { return !!sLpcKey && this.hasRows(aRows); },
        isBarriersVisibleByLpc: function (sLpcKey) { return !!sLpcKey; },
        formatHeaderDate: DetailFormatters.formatHeaderDate,
        formatInfoCardValue: DetailFormatters.formatInfoCardValue,
        formatPersonSuggestion: DetailFormatters.formatPersonSuggestion,
        formatAttachmentSize: DetailFormatters.formatAttachmentSize,
        formatAttachmentUploadHint: function (aExtensions, iMaxSizeMb) {
            return AttachmentUploadSupport.formatUploadHint(this, aExtensions, iMaxSizeMb);
        },
        formatI18nByKey: DetailFormatters.formatI18nByKey,
        formatHeartbeatText: DetailFormatters.formatHeartbeatText,
        formatAutosaveAt: DetailFormatters.formatAutosaveAt,
        formatCopyLinkVisible: DetailFormatters.formatCopyLinkVisible,
        formatDeleteChecklistVisible: DetailFormatters.formatDeleteChecklistVisible,
        formatAutosaveText: DetailFormatters.formatAutosaveText
    });

    return BaseController.extend("checklist.app.controller.Detail", mControllerDefinition);
});
