sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailControllerBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationSummaryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionViewportBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionPinnedRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailChecklistBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailChecklistStateBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailChecklistRowBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailAttachmentLocationBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFormatters",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore"
], function (
    BaseController,
    DetailControllerBehavior,
    DetailValidationSummaryRuntime,
    DetailActionConstants,
    DetailActionViewportBehavior,
    DetailActionPinnedRailRuntime,
    DetailActionDialogRuntime,
    DetailChecklistBehavior,
    DetailChecklistStateBehavior,
    DetailChecklistRowBehavior,
    DetailAttachmentLocationBehavior,
    DetailFormatters,
    AttachmentUploadCore
) {
    "use strict";
    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    var mControllerDefinition = Object.assign(
        {},
        DetailControllerBehavior,
        DetailActionViewportBehavior,
        DetailActionPinnedRailRuntime,
        DetailActionDialogRuntime,
        DetailChecklistBehavior,
        DetailChecklistStateBehavior,
        DetailChecklistRowBehavior,
        DetailAttachmentLocationBehavior,
        {
            _computeValidationSummary: function () {
            return DetailValidationSummaryRuntime.compute(this);
            },

            _recomputeValidationSummary: function (sSource, bShowValidation) {
            return DetailValidationSummaryRuntime.recompute(this, sSource, bShowValidation, STATE_PATHS);
            },

            _focusFirstInvalidField: function () {
            return DetailValidationSummaryRuntime.focusFirstInvalidField(this, STATE_PATHS);
            },

            _onSelectedChecklistChanged: function (oEvent) {
            DetailValidationSummaryRuntime.onSelectedChecklistChanged(this, oEvent, STATE_PATHS);
            }
        },
        {
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
        formatLockStateText: DetailFormatters.formatLockStateText,
        formatLockStateSemantic: DetailFormatters.formatLockStateSemantic,
        formatPassedTotal: DetailFormatters.formatPassedTotal,
        formatRowCount: DetailFormatters.formatRowCount,
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
        formatPersonSuggestionMeta: DetailFormatters.formatPersonSuggestionMeta,
        formatAttachmentSize: DetailFormatters.formatAttachmentSize,
        formatAttachmentUploadHint: function (aExtensions, iMaxSizeMb) {
            return AttachmentUploadCore.formatUploadHint(this, aExtensions, iMaxSizeMb);
        },
        formatI18nByKey: DetailFormatters.formatI18nByKey,
        formatHeartbeatText: DetailFormatters.formatHeartbeatText,
        formatAutosaveAt: DetailFormatters.formatAutosaveAt,
        formatPersistenceState: DetailFormatters.formatPersistenceState,
        formatPersistenceText: DetailFormatters.formatPersistenceText,
        formatPersistenceTooltip: DetailFormatters.formatPersistenceTooltip,
        formatCopyLinkVisible: DetailFormatters.formatCopyLinkVisible,
        formatDeleteChecklistVisible: DetailFormatters.formatDeleteChecklistVisible,
        formatAutosaveText: DetailFormatters.formatAutosaveText,
        formatAttachmentsEmptyStateText: DetailFormatters.formatAttachmentsEmptyStateText
    });

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Detail", mControllerDefinition);
});
