sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailControllerBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationSummaryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionConstants",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionViewportBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionPinnedRailRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailChecklistBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailInteractionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFormatters",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBindingRuntime"
], function (
    DetailControllerBehavior,
    DetailValidationSummaryRuntime,
    DetailActionConstants,
    DetailActionViewportBehavior,
    DetailActionPinnedRailRuntime,
    DetailActionDialogRuntime,
    DetailChecklistBehavior,
    DetailInteractionRuntime,
    DetailFormatters,
    DetailAttachmentViewState,
    AttachmentUploadCore,
    DetailRowBindingRuntime
) {
    "use strict";

    var STATE_PATHS = DetailActionConstants.STATE_PATHS;

    function hasRows(aRows) {
        return Array.isArray(aRows) && aRows.length > 0;
    }

    return Object.assign(
        {},
        DetailControllerBehavior,
        DetailActionViewportBehavior,
        DetailActionPinnedRailRuntime,
        DetailActionDialogRuntime,
        DetailChecklistBehavior,
        DetailInteractionRuntime,
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
                DetailAttachmentViewState.sync(this);
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
            hasRows: hasRows,
            isEmptyRows: function (aRows) {
                return !hasRows(aRows);
            },
            isDesktopTableVisible: function (bPhone, bNarrow, aRows) {
                return !bPhone && !bNarrow && hasRows(aRows);
            },
            isPhoneTableVisible: function (bPhone, bNarrow, aRows) {
                return !!(bPhone || bNarrow) && hasRows(aRows);
            },
            isRuntimeDesktopTableVisible: function (bPhone, bNarrow, aRows, bIgnoreNarrowViewport) {
                return !bPhone && (!!bIgnoreNarrowViewport || !bNarrow) && hasRows(aRows);
            },
            isRuntimePhoneTableVisible: function (bPhone, bNarrow, aRows, bIgnoreNarrowViewport) {
                return (!!bPhone || (!bIgnoreNarrowViewport && !!bNarrow)) && hasRows(aRows);
            },
            isExpandedDialogBusy: function (sKind, bChecksBusy, bBarriersBusy) {
                return String(sKind || "").trim() === "barrier" ? !!bBarriersBusy : !!bChecksBusy;
            },
            formatDetailRowNumber: function (oRow, sKind) {
                return DetailRowBindingRuntime.formatRowNumber(oRow, sKind);
            },
            onChecklistRowsBindingChange: function (oEvent) {
                var oSource = oEvent && oEvent.getSource && oEvent.getSource();
                var oViewContext = oSource && oSource.getBindingContext && oSource.getBindingContext("view");
                var oSpec = oViewContext && oViewContext.getObject && oViewContext.getObject();
                DetailRowBindingRuntime.bindSelectedCollectionContext(oSource, oSpec);
            },
            isBarriersRatioVisible: function (sLpcKey, aRows) {
                return !!sLpcKey && hasRows(aRows);
            },
            isBarriersVisibleByLpc: function (sLpcKey) {
                return !!sLpcKey;
            },
            formatHeaderDate: DetailFormatters.formatHeaderDate,
            formatInfoCardValue: DetailFormatters.formatInfoCardValue,
            formatPersonSuggestion: DetailFormatters.formatPersonSuggestion,
            formatPersonSuggestionMeta: DetailFormatters.formatPersonSuggestionMeta,
            formatAttachmentSize: DetailFormatters.formatAttachmentSize,
            formatAttachmentUploadHint: function (aExtensions, iMaxSizeMb) {
                var oResourceBundle = this.getResourceBundle ? this.getResourceBundle() : null;
                return AttachmentUploadCore.formatUploadHint(oResourceBundle, aExtensions, iMaxSizeMb);
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
            formatAttachmentsEmptyStateText: DetailFormatters.formatAttachmentsEmptyStateText,
            isCheckSectionEmpty: function (sKind, aRows) {
                return String(sKind || "").trim() === "check" && !hasRows(aRows);
            },
            isBarrierSectionEmpty: function (sKind, aRows) {
                return String(sKind || "").trim() === "barrier" && !hasRows(aRows);
            },
            hasCheckSectionRows: function (sKind, aRows) {
                return String(sKind || "").trim() === "check" && hasRows(aRows);
            },
            hasBarrierSectionRows: function (sKind, aRows) {
                return String(sKind || "").trim() === "barrier" && hasRows(aRows);
            }
        }
    );
});
