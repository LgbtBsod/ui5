sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailControllerBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailValidationSummaryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailChecklistBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentDropZoneRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/AttachmentUploadCore",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailInteractionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailObserverCardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPersonInputRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailSimpleCardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerReturnFocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailFormatters",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailRowBindingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimePayloadNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "sap/ui/model/odata/OperationMode",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (
    BaseController,
    DetailControllerBehavior,
    DetailValidationSummaryRuntime,
    DetailChecklistBehavior,
    AttachmentDropZoneRuntime,
    AttachmentUploadCore,
    DetailInteractionRuntime,
    DetailObserverCardRuntime,
    DetailPersonInputRuntime,
    DetailSimpleCardRuntime,
    ControllerReturnFocusRuntime,
    DetailFormatters,
    DetailAttachmentViewState,
    DetailRowBindingRuntime,
    RuntimePayloadNormalizer,
    ControllerViewStateRuntime,
    FocusRuntime,
    SchedulingRuntime,
    OperationMode,
    JsRuntime
) {
    "use strict";

    var STATE_PATHS = Object.freeze({
        SAVE_IN_FLIGHT: "/saveInFlight",
        VALIDATION_SUMMARY: "/validationSummary"
    });
    var DETAIL_NARROW_VIEWPORT_REM = 70;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function hasRows(aRows) {
        return Array.isArray(aRows) && aRows.length > 0;
    }

    function remToPx(fRem) {
        var iRootSize = parseFloat(window.getComputedStyle(document.documentElement).fontSize || "16");
        return Math.round(Number(fRem || 0) * (Number.isFinite(iRootSize) && iRootSize > 0 ? iRootSize : 16));
    }

    function syncAdaptiveDetailViewport(oController) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION && oController.getView();
        var oObjectPage = oController.byId("detailObjectPage");
        var oDom = (oObjectPage && typeof oObjectPage[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oObjectPage[METHODS.GET_DOM_REF]())
            || (oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oView[METHODS.GET_DOM_REF]());
        var iWidth;
        var bNarrow;
        if (!oDom) {
            return;
        }
        iWidth = Math.round((oDom.getBoundingClientRect && oDom.getBoundingClientRect().width) || 0);
        bNarrow = iWidth > 0 && iWidth <= remToPx(DETAIL_NARROW_VIEWPORT_REM);
        ControllerViewStateRuntime.setFlag(oController, "/narrowDetailViewport", bNarrow);
        DetailAttachmentViewState.sync(oController);
        if (oView && typeof oView.toggleStyleClass === TYPE_FUNCTION) {
            oView.toggleStyleClass("detailViewportNarrow", bNarrow);
        }
        if (oController && typeof oController._syncViewportPinnedControlRail === TYPE_FUNCTION) {
            oController._syncViewportPinnedControlRail();
        }
    }

    function bindAdaptiveDetailViewport(oController) {
        var oView = oController && typeof oController.getView === TYPE_FUNCTION && oController.getView();
        var oDom = oView && typeof oView[METHODS.GET_DOM_REF] === TYPE_FUNCTION && oView[METHODS.GET_DOM_REF]();
        if (!oDom) {
            return;
        }
        if (!oController._fnAdaptiveViewportSync) {
            oController._fnAdaptiveViewportSync = syncAdaptiveDetailViewport.bind(null, oController);
        }
        unbindAdaptiveDetailViewport(oController);
        
        if (typeof ResizeObserver === TYPE_FUNCTION) {
            if (oController._oAdaptiveViewportResizeObserver) {
                oController._oAdaptiveViewportResizeObserver.disconnect();
            }
            oController._oAdaptiveViewportResizeObserver = new ResizeObserver(oController._fnAdaptiveViewportSync);
            oController._oAdaptiveViewportResizeObserver.observe(oDom);
        }
        
        window.addEventListener("resize", oController._fnAdaptiveViewportSync, true);
        syncAdaptiveDetailViewport(oController);
    }

    function unbindAdaptiveDetailViewport(oController) {
        if (oController._oAdaptiveViewportResizeObserver && typeof oController._oAdaptiveViewportResizeObserver.disconnect === TYPE_FUNCTION) {
            oController._oAdaptiveViewportResizeObserver.disconnect();
            oController._oAdaptiveViewportResizeObserver = null;
        }
        if (oController._fnAdaptiveViewportSync) {
            window.removeEventListener("resize", oController._fnAdaptiveViewportSync, true);
            oController._fnAdaptiveViewportSync = null;
        }
    }

    function bindViewportPinnedControlRail(oController) {
        var oStickyHost = (oController.byId && (oController.byId("detailControlPinnedDock") || oController.byId("detailControlStickyHost"))) || null;
        var oHostDom = oStickyHost && oStickyHost.getDomRef && oStickyHost.getDomRef();
        if (!oHostDom) {
            return;
        }
        oHostDom.style.removeProperty("height");
        oHostDom.style.removeProperty("--detail-rail-height");
    }

    function unbindViewportPinnedControlRail(oController) {
        var oSwitch = oController.byId && oController.byId("detailEditSwitch");
        if (oSwitch && oSwitch.removeEventDelegate && oController._oDetailEditSwitchDelegate) {
            oSwitch.removeEventDelegate(oController._oDetailEditSwitchDelegate, oController);
            oController._oDetailEditSwitchDelegate = null;
        }
    }

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.Detail", Object.assign(
        {},
        DetailControllerBehavior,
        DetailChecklistBehavior,
        {
            _dispatchDetailCommand: function (sMethod, mInput) {
                return DetailChecklistBehavior._dispatchDetailCommand.call(
                    this,
                    sMethod,
                    RuntimePayloadNormalizer.normalize(mInput || {})
                );
            },
            _withViewFlag: function (sPath, fnWork) {
                return ControllerViewStateRuntime.withFlag(this, sPath, fnWork, true, false);
            },
            _clearLocationValueHelpSearchTimer: function () {
                this._iLocationVhSearchTimer = SchedulingRuntime.clearTimer(this._iLocationVhSearchTimer);
            },
            _scheduleLocationValueHelpTableSync: function () {
                this._iLocationVhTableSyncTimer = SchedulingRuntime.restartTimer(this._iLocationVhTableSyncTimer, function () {
                    var oTreeTable = this.byId("locationValueHelpTreeTable");
                    this._iLocationVhTableSyncTimer = null;
                    if (!oTreeTable) {
                        return;
                    }
                    if (typeof oTreeTable.clearSelection === TYPE_FUNCTION) {
                        oTreeTable.clearSelection();
                    }
                    if (typeof oTreeTable.setFirstVisibleRow === TYPE_FUNCTION) {
                        oTreeTable.setFirstVisibleRow(0);
                    }
                    if (typeof oTreeTable.invalidate === TYPE_FUNCTION) {
                        oTreeTable.invalidate();
                    }
                }.bind(this), 90);
            },
            _rememberDialogReturnFocus: function (sKey, oControl) {
                var oFallback = oControl;
                if (!sKey) {
                    return;
                }
                if (!oFallback) {
                    oFallback = FocusRuntime.createActiveElementFallback();
                }
                ControllerReturnFocusRuntime.remember(this, sKey, oFallback, {
                    storeProperty: "_mDialogReturnFocus"
                });
            },
            _restoreDialogFocus: function (sKey) {
                ControllerReturnFocusRuntime.restore(this, sKey, {
                    storeProperty: "_mDialogReturnFocus"
                });
            },
            _onDialogAfterOpen: function (sKey) {
                var oSearchField;
                var oFocusTarget;
                if (sKey !== "locationValueHelp") {
                    return;
                }
                oSearchField = this.byId("locationValueHelpSearchField");
                SchedulingRuntime.restartTimer(0, function () {
                    oFocusTarget = oSearchField && typeof oSearchField[METHODS.GET_FOCUS_DOM_REF] === TYPE_FUNCTION ? oSearchField[METHODS.GET_FOCUS_DOM_REF]() : null;
                    if (oFocusTarget && typeof oFocusTarget[METHODS.FOCUS] === TYPE_FUNCTION) {
                        oFocusTarget[METHODS.FOCUS]();
                        return;
                    }
                    FocusRuntime.focusSoon(oSearchField);
                }, 60);
            },
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
                bindAdaptiveDetailViewport(this);
            },
            _unbindAdaptiveDetailViewport: function () {
                unbindAdaptiveDetailViewport(this);
            },
            _syncAdaptiveDetailViewport: function () {
                syncAdaptiveDetailViewport(this);
            },
            _clearViewportPinnedControlRailRetry: function () {},
            _scheduleViewportPinnedControlRailBind: function () {
                bindViewportPinnedControlRail(this);
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
                unbindViewportPinnedControlRail(this);
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
            onOpenChecksNumberValueHelp: function (oEvent) {
                return DetailInteractionRuntime.onOpenChecksNumberValueHelp.call(this, oEvent);
            },
            onOpenBarriersNumberValueHelp: function (oEvent) {
                return DetailInteractionRuntime.onOpenBarriersNumberValueHelp.call(this, oEvent);
            },
            onOpenLocationValueHelp: function (oEvent) {
                return DetailInteractionRuntime.onOpenLocationValueHelp.call(this, oEvent);
            },
            onLocationValueHelpSearch: function (oEvent) {
                return DetailInteractionRuntime.onLocationValueHelpSearch.call(this, oEvent);
            },
            onLocationValueHelpSearchSubmit: function (oEvent) {
                return DetailInteractionRuntime.onLocationValueHelpSearchSubmit.call(this, oEvent);
            },
            onLocationTreeSelectionChange: function (oEvent) {
                return DetailInteractionRuntime.onLocationTreeSelectionChange.call(this, oEvent);
            },
            onConfirmLocationValueHelp: function () {
                return DetailInteractionRuntime.onConfirmLocationValueHelp.call(this);
            },
            onCloseLocationValueHelp: function () {
                return DetailInteractionRuntime.onCloseLocationValueHelp.call(this);
            },
            onChecksNumberChange: function (oEvent) {
                return DetailInteractionRuntime.onChecksNumberChange.call(this, oEvent);
            },
            onBarriersNumberChange: function (oEvent) {
                return DetailInteractionRuntime.onBarriersNumberChange.call(this, oEvent);
            },
            onLpcChange: function (oEvent) {
                return DetailInteractionRuntime.onLpcChange.call(this, oEvent);
            },
            onProfessionChange: function (oEvent) {
                return DetailInteractionRuntime.onProfessionChange.call(this, oEvent);
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
                if (this.getModel && this.getModel("view")) {
                    ControllerViewStateRuntime.set(this, "/barriersVisible", this.isBarriersVisibleByLpc(
                        this.getModel("detail") && this.getModel("detail").getProperty
                            ? this.getModel("detail").getProperty("/current/basic/LPC_KEY")
                            : ""
                    ));
                }
                DetailValidationSummaryRuntime.onDetailModelChanged(this, oEvent, STATE_PATHS);
            },
            onExit: function () {
                unbindAdaptiveDetailViewport(this);
                unbindViewportPinnedControlRail(this);
                BaseController.prototype.onExit.apply(this, arguments);
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
    ));
});
