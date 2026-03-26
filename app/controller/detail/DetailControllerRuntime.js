sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailControllerBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationSummaryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailActionDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailChecklistBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentDropZoneRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailAdaptiveViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailInteractionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailObserverCardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailSimpleCardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFormatters",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBindingRuntime"
], function (
    DetailControllerBehavior,
    DetailValidationSummaryRuntime,
    DetailActionDialogRuntime,
    DetailChecklistBehavior,
    AttachmentDropZoneRuntime,
    AttachmentUploadCore,
    DetailAdaptiveViewportRuntime,
    DetailInteractionRuntime,
    DetailObserverCardRuntime,
    DetailPersonInputRuntime,
    DetailSimpleCardRuntime,
    DetailFormatters,
    DetailAttachmentViewState,
    DetailRowBindingRuntime
) {
    "use strict";

    var STATE_PATHS = Object.freeze({
        SAVE_IN_FLIGHT: "/saveInFlight",
        VALIDATION_SUMMARY: "/validationSummary"
    });

    function hasRows(aRows) {
        return Array.isArray(aRows) && aRows.length > 0;
    }

    return Object.assign(
        {},
        DetailControllerBehavior,
        DetailActionDialogRuntime,
        DetailChecklistBehavior,
        {
            _bindAttachmentDropZone: function () {
                AttachmentDropZoneRuntime.bindAttachmentDropZone(this);
            },
            _unbindAttachmentDropZone: function () {
                AttachmentDropZoneRuntime.unbindAttachmentDropZone(this);
            },
            _scheduleAttachmentDropZoneBind: function (iAttempt) {
                AttachmentDropZoneRuntime.scheduleAttachmentDropZoneBind(this, iAttempt);
            },
            _bindAdaptiveDetailViewport: function () {
                DetailAdaptiveViewportRuntime.bindAdaptiveDetailViewport(this);
            },
            _unbindAdaptiveDetailViewport: function () {
                DetailAdaptiveViewportRuntime.unbindAdaptiveDetailViewport(this);
            },
            _syncAdaptiveDetailViewport: function () {
                DetailAdaptiveViewportRuntime.syncAdaptiveDetailViewport(this);
            },
            _clearViewportPinnedControlRailRetry: function () {},
            _scheduleViewportPinnedControlRailBind: function () {
                var oStickyHost = (this.byId && (this.byId("detailControlPinnedDock") || this.byId("detailControlStickyHost"))) || null;
                var oHostDom = oStickyHost && oStickyHost.getDomRef && oStickyHost.getDomRef();
                if (!oHostDom) {
                    return;
                }
                oHostDom.style.removeProperty("height");
                oHostDom.style.removeProperty("--detail-rail-height");
            },
            _bindDetailEditSwitchKeyboardFallback: function () {
                var oSwitch = this.byId && this.byId("detailEditSwitch");
                if (!oSwitch || !oSwitch.addEventDelegate) {
                    return;
                }
                if (!this._oDetailEditSwitchDelegate) {
                    this._oDetailEditSwitchDelegate = {
                        onsapenter: this._onDetailEditSwitchKeyboardActivate.bind(this),
                        onsapspace: this._onDetailEditSwitchKeyboardActivate.bind(this)
                    };
                }
                oSwitch.addEventDelegate(this._oDetailEditSwitchDelegate, this);
            },
            _unbindViewportPinnedControlRail: function () {
                var oSwitch = this.byId && this.byId("detailEditSwitch");
                if (oSwitch && oSwitch.removeEventDelegate && this._oDetailEditSwitchDelegate) {
                    oSwitch.removeEventDelegate(this._oDetailEditSwitchDelegate, this);
                }
            },
            _bindViewportPinnedControlRail: function () {
                this._scheduleViewportPinnedControlRailBind();
            },
            _syncViewportPinnedControlRail: function () {
                this._scheduleViewportPinnedControlRailBind();
            },
            _onDetailEditSwitchKeyboardActivate: function (oEvent) {
                var oSwitch = this.byId && this.byId("detailEditSwitch");
                if (!oSwitch || !oSwitch.getEnabled || !oSwitch.getEnabled()) {
                    return;
                }
                if (oEvent && oEvent.preventDefault) {
                    oEvent.preventDefault();
                }
                if (oEvent && oEvent.stopPropagation) {
                    oEvent.stopPropagation();
                }
                oSwitch.fireChange({ state: !oSwitch.getState() });
            },
            onAttachmentUploadChange: function (oEvent) {
                return AttachmentUploadCore.onUploaderChange(this, oEvent);
            },
            onDeleteAttachment: function (oEvent) {
                return DetailInteractionRuntime.attachmentDelete(this, oEvent);
            },
            onOpenAttachment: function (oEvent) {
                return DetailInteractionRuntime.attachmentLoad(this, oEvent);
            },
            onOpenAttachmentPicker: function () {
                return AttachmentUploadCore.openNativeFilePicker(this);
            },
            onOpenWorkflowAnalytics: function () {
                return DetailInteractionRuntime.openWorkflowAnalytics(this);
            },
            onToggleAttachmentsSection: function () {
                return DetailInteractionRuntime.toggleAttachmentsSection(this);
            },
            onPersonInputChange: function (oEvent) {
                return DetailPersonInputRuntime.onPersonInputChange(this, oEvent);
            },
            onPersonSuggest: function (oEvent) {
                return DetailPersonInputRuntime.onPersonSuggest(this, oEvent);
            },
            onPersonSuggestionSelected: function (oEvent) {
                return DetailPersonInputRuntime.onPersonSuggestionSelected(this, oEvent);
            },
            buildObserverCard: function (sId, oContext) {
                return DetailObserverCardRuntime.createContent(this, sId, oContext);
            },
            buildSimpleCard: function (sId, oContext) {
                return DetailSimpleCardRuntime.createContent(this, sId, oContext);
            },
            _computeValidationSummary: function () {
                return DetailValidationSummaryRuntime.compute(this);
            },
            _recomputeValidationSummary: function (sSource, bShowValidation) {
                return DetailValidationSummaryRuntime.recompute(this, sSource, bShowValidation, STATE_PATHS);
            },
            _focusFirstInvalidField: function () {
                return DetailValidationSummaryRuntime.focusFirstInvalidField(this, STATE_PATHS);
            },
            _onDetailModelChanged: function (oEvent) {
                DetailAttachmentViewState.sync(this);
                DetailValidationSummaryRuntime.onDetailModelChanged(this, oEvent, STATE_PATHS);
            }
        },
        {
            formatValidationState: DetailFormatters.formatValidationState,
            formatValidationText: DetailFormatters.formatValidationText,
            formatWarningMessageType: DetailFormatters.formatWarningMessageType,
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
                DetailRowBindingRuntime.bindDetailCollectionContext(oSource, oSpec);
            },
            isBarriersRatioVisible: function (sLpcKey, aRows) {
                return !!sLpcKey && hasRows(aRows);
            },
            isBarriersVisibleByLpc: function (sLpcKey) {
                return !!sLpcKey;
            },
            isAttachmentSectionEmpty: function (aPersisted, aSession) {
                return !hasRows(aPersisted) && !hasRows(aSession);
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
            formatPersistedActionVisible: DetailFormatters.formatPersistedActionVisible,
            formatEditActionVisible: DetailFormatters.formatEditActionVisible,
            formatEditSwitchState: DetailFormatters.formatEditSwitchState,
            formatEditSwitchEnabled: DetailFormatters.formatEditSwitchEnabled,
            formatDeleteChecklistVisible: DetailFormatters.formatDeleteChecklistVisible,
            formatDeleteChecklistConfirmVisible: DetailFormatters.formatDeleteChecklistConfirmVisible,
            formatAutosaveText: DetailFormatters.formatAutosaveText,
            formatAttachmentsEmptyStateText: DetailFormatters.formatAttachmentsEmptyStateText,
            formatIntegrationVisible: DetailFormatters.formatIntegrationVisible,
            formatIntegrationText: DetailFormatters.formatIntegrationText,
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
