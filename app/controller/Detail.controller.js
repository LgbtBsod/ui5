sap.ui.define([
    "checklist/app/controller/Base.controller",
    "checklist/app/controller/support/DetailControllerLifecycle",
    "checklist/app/controller/support/DetailValidationSummarySupport",
    "checklist/app/controller/support/DetailActionConstants",
    "checklist/app/controller/support/DetailActionViewportSupport",
    "checklist/app/controller/support/DetailActionPinnedRailSupport",
    "checklist/app/controller/support/DetailActionDialogSupport",
    "checklist/app/controller/support/DetailChecklistCoreSupport",
    "checklist/app/controller/support/DetailChecklistStateActions",
    "checklist/app/controller/support/DetailChecklistRowActions",
    "checklist/app/controller/support/DetailAttachmentLocationActions",
    "checklist/app/controller/support/DetailFormatters",
    "checklist/app/controller/support/AttachmentUploadCore"
], function (
    BaseController,
    DetailControllerLifecycle,
    DetailValidationSummarySupport,
    DetailActionConstants,
    DetailActionViewportSupport,
    DetailActionPinnedRailSupport,
    DetailActionDialogSupport,
    DetailChecklistCoreSupport,
    DetailChecklistStateActions,
    DetailChecklistRowActions,
    DetailAttachmentLocationActions,
    DetailFormatters,
    AttachmentUploadCore
) {
    "use strict";
    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    var mControllerDefinition = Object.assign(
        {},
        DetailControllerLifecycle,
        DetailActionViewportSupport,
        DetailActionPinnedRailSupport,
        DetailActionDialogSupport,
        DetailChecklistCoreSupport,
        DetailChecklistStateActions,
        DetailChecklistRowActions,
        DetailAttachmentLocationActions,
        {
            _computeValidationSummary: function () {
                return DetailValidationSummarySupport.compute(this);
            },

            _recomputeValidationSummary: function (sSource, bShowValidation) {
                return DetailValidationSummarySupport.recompute(this, sSource, bShowValidation, STATE_PATHS);
            },

            _focusFirstInvalidField: function () {
                return DetailValidationSummarySupport.focusFirstInvalidField(this, STATE_PATHS);
            },

            _onSelectedChecklistChanged: function (oEvent) {
                DetailValidationSummarySupport.onSelectedChecklistChanged(this, oEvent, STATE_PATHS);
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
        formatPersonSuggestionMeta: DetailFormatters.formatPersonSuggestionMeta,
        formatAttachmentSize: DetailFormatters.formatAttachmentSize,
        formatAttachmentUploadHint: function (aExtensions, iMaxSizeMb) {
            return AttachmentUploadCore.formatUploadHint(this, aExtensions, iMaxSizeMb);
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
