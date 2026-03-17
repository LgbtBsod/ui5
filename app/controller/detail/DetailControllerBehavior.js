sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/DetailRuntimeContracts"
], function (
    ControllerResourceCleanup,
    DetailFacade,
    ControllerRouteRuntime,
    ControllerTextRuntime,
    DetailAccessViewState,
    DetailInfoCardLayoutRuntime,
    StatePaths,
    ControllerModelRuntime,
    ControllerViewStateRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    NavigationContracts,
    ModelContracts,
    DetailRuntimeContracts
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var STATE_MODEL = MODELS.STATE;
    var SELECTED_MODEL = MODELS.SELECTED;
    var INFO_CARD_KEYS = DetailRuntimeContracts.INFO_CARD_KEYS;
    var INFO_CARD_TEXT_KEYS = DetailRuntimeContracts.INFO_CARD_TEXT_KEYS;
    var INFO_CARD_TEXT_FALLBACKS = DetailRuntimeContracts.INFO_CARD_TEXT_FALLBACKS;
    var VIEW_DEFAULTS = DetailRuntimeContracts.VIEW_DEFAULTS;

    function parseInitialDetailHash() {
        var sHash = String((typeof window !== "undefined" && window.location && window.location.hash) || "").trim();
        var oMatch;
        if (!sHash) {
            return null;
        }
        sHash = sHash.replace(/^#\/?/, "");
        oMatch = /^checklist\/([^\/?#]+)(?:\/([^\/?#]+))?$/i.exec(sHash);
        if (!oMatch) {
            return null;
        }
        return {
            id: decodeURIComponent(oMatch[1] || ""),
            layout: decodeURIComponent(oMatch[2] || "")
        };
    }

    function buildInitialViewState(oController) {
        var fnText = function (sKey, sFallback) {
            return ControllerTextRuntime.getText(oController, sKey, [], sFallback || sKey);
        };
        return {
            detailSkeletonBusy: false,
            attachmentBusy: false,
            attachmentsExpanded: false,
            attachmentsLoaded: false,
            sessionAttachments: [],
            checksBusy: false,
            barriersBusy: false,
            checksExpandedBusy: false,
            barriersExpandedBusy: false,
            locationVhBusy: false,
            attachmentCategoryKey: VIEW_DEFAULTS.ATTACHMENT_CATEGORY_KEY,
            observerSuggestions: [],
            observedSuggestions: [],
            observerInputValue: "",
            observedInputValue: "",
            personSuggestHint: "",
            locationVhHint: "",
            narrowDetailViewport: false,
            deleteChecklistConfirmArmed: false,
            accessState: DetailAccessViewState.createDefaultState(""),
            validationShown: false,
            validationMissing: {},
            infoCards: DetailInfoCardLayoutRuntime.resolveCards(oController, [
                { key: INFO_CARD_KEYS.DATETIME, title: fnText(INFO_CARD_TEXT_KEYS.DATETIME, INFO_CARD_TEXT_FALLBACKS.DATETIME), pinned: true },
                { key: INFO_CARD_KEYS.LOCATION, title: fnText(INFO_CARD_TEXT_KEYS.LOCATION, INFO_CARD_TEXT_FALLBACKS.LOCATION), pinned: true },
                { key: INFO_CARD_KEYS.EQUIPMENT, title: fnText(INFO_CARD_TEXT_KEYS.EQUIPMENT, INFO_CARD_TEXT_FALLBACKS.EQUIPMENT), pinned: true },
                { key: INFO_CARD_KEYS.OBSERVER, title: fnText(INFO_CARD_TEXT_KEYS.OBSERVER, INFO_CARD_TEXT_FALLBACKS.OBSERVER), pinned: true },
                { key: INFO_CARD_KEYS.OBSERVED, title: fnText(INFO_CARD_TEXT_KEYS.OBSERVED, INFO_CARD_TEXT_FALLBACKS.OBSERVED), pinned: true },
                { key: INFO_CARD_KEYS.PROFESSION, title: fnText(INFO_CARD_TEXT_KEYS.PROFESSION, INFO_CARD_TEXT_FALLBACKS.PROFESSION), pinned: false },
                { key: INFO_CARD_KEYS.LPC, title: fnText(INFO_CARD_TEXT_KEYS.LPC, INFO_CARD_TEXT_FALLBACKS.LPC), pinned: false },
                { key: INFO_CARD_KEYS.CRITERIA_NUMBERS, title: fnText(INFO_CARD_TEXT_KEYS.CRITERIA_NUMBERS, INFO_CARD_TEXT_FALLBACKS.CRITERIA_NUMBERS), pinned: false }
            ])
        };
    }

    function bindStateValidationModel(oController) {
        oController._oStateValidationModel = ControllerModelRuntime.state(oController);
        if (!oController._oStateValidationModel || !oController._oStateValidationModel.attachPropertyChange) {
            return;
        }
        oController._fnStateValidationChange = function (oEvent) {
            var sPath = oEvent && oEvent.getParameter && oEvent.getParameter("path");
            var oViewModel;
            if (sPath === "/requiredFields") {
                oController._recomputeValidationSummary("requiredFieldsChanged", false);
                return;
            }
            if (sPath !== StatePaths.WORKFLOW_DETAIL_EDIT_MODE && sPath !== "/activeObjectId" && sPath !== "/selectedId") {
                return;
            }
            oViewModel = ControllerModelRuntime.viewState(oController);
            if (oViewModel) {
                ControllerViewStateRuntime.set(oController, "/deleteChecklistConfirmArmed", false);
            }
        };
        oController._oStateValidationModel.attachPropertyChange(oController._fnStateValidationChange, oController);
    }

    function currentHash() {
        return String((typeof window !== "undefined" && window.location && window.location.hash) || "");
    }

    function toggleCloseFallbackListeners(oDomRef, fnHandler, bAttach) {
        var aEvents = ["click", "pointerup", "mouseup", "touchend"];
        if (!oDomRef || !fnHandler) {
            return;
        }
        aEvents.forEach(function (sEventName) {
            if (bAttach) {
                oDomRef.addEventListener(sEventName, fnHandler, true);
                return;
            }
            oDomRef.removeEventListener(sEventName, fnHandler, true);
        });
    }

    return {
        onInit: function () {
            this._facade = new DetailFacade();
            this._bDetailInitialRouteHandled = false;
            this._mLazyDialogs = {};
            this._mDialogReturnFocus = {};
            this._iAttachmentDropZoneBindTimer = null;
            this._iLocationVhSearchTimer = null;
            this._iLocationVhTableSyncTimer = null;
            this._oDetailScrollHost = null;
            this._fnDetailScrollSync = null;
            this._fnDetailResizeSync = null;
            this._oAdaptiveViewportResizeObserver = null;
            this._fnAdaptiveViewportSync = null;
            ControllerRouteRuntime.attachMatched(this, [
                { name: NavigationContracts.ROUTES.DETAIL, handler: this._onDetailMatched },
                { name: NavigationContracts.ROUTES.DETAIL_LAYOUT, handler: this._onDetailMatched },
                { name: NavigationContracts.ROUTES.SEARCH, handler: this._onDetailRouteLeave },
                { name: NavigationContracts.ROUTES.ANALYTICS, handler: this._onDetailRouteLeave }
            ]);

            ControllerViewStateRuntime.initModel(this, buildInitialViewState.bind(null, this));

            this._oSelectedModel = ControllerModelRuntime.selected(this);
            if (this._oSelectedModel && this._oSelectedModel.attachPropertyChange) {
                this._oSelectedModel.attachPropertyChange(this._onSelectedChecklistChanged, this);
            }
            bindStateValidationModel(this);
        },

        onAfterRendering: function () {
            this._scheduleAttachmentDropZoneBind();
            this._bindDetailEditSwitchKeyboardFallback();
            this._bindDetailCloseButtonFallback();
            this._bindAdaptiveDetailViewport();
            this._bindViewportPinnedControlRail();
            this._replayInitialDetailRouteIfNeeded();
        },

        onExit: function () {
            if (this._oSelectedModel && this._oSelectedModel.detachPropertyChange) {
                this._oSelectedModel.detachPropertyChange(this._onSelectedChecklistChanged, this);
            }
            if (this._oStateValidationModel && this._fnStateValidationChange && this._oStateValidationModel.detachPropertyChange) {
                this._oStateValidationModel.detachPropertyChange(this._fnStateValidationChange, this);
            }
            ControllerRouteRuntime.detachAllMatched(this);
            this._iAttachmentDropZoneBindTimer = SchedulingRuntime.clearTimer(this._iAttachmentDropZoneBindTimer);
            this._iDetailCloseFallbackTimer = SchedulingRuntime.clearTimer(this._iDetailCloseFallbackTimer);
            this._clearLocationValueHelpSearchTimer();
            this._iLocationVhTableSyncTimer = SchedulingRuntime.clearTimer(this._iLocationVhTableSyncTimer);
            this._unbindDetailCloseButtonFallback();
            this._unbindViewportPinnedControlRail();
            this._unbindAttachmentDropZone();
            ControllerResourceCleanup.destroyMapEntries(this._mLazyDialogs);
            this._oSelectedModel = null;
            this._mLazyDialogs = null;
            this._mDialogReturnFocus = null;
            this._oAdaptiveViewportResizeObserver = null;
            this._fnAdaptiveViewportSync = null;
            this._oStateValidationModel = null;
            this._fnStateValidationChange = null;
            this._bDetailInitialRouteHandled = null;
        },

        _bindDetailCloseButtonFallback: function () {
            var oView = this.getView && this.getView();
            var oViewDom = oView && oView.getDomRef && oView.getDomRef();
            if (!oViewDom) {
                return;
            }
            this._unbindDetailCloseButtonFallback();
            this._fnDetailCloseBrowserClick = function (oEvent) {
                var oTarget = oEvent && oEvent.target;
                var sHashBefore = currentHash();
                var sRouteBefore = String(ModelStateRuntime.read(this, STATE_MODEL, "/currentRouteName", "") || "");
                if (!oTarget || !oTarget.closest || !oTarget.closest(".detailRailCloseAction")) {
                    return;
                }
                this._iDetailCloseFallbackTimer = SchedulingRuntime.restartTimer(this._iDetailCloseFallbackTimer, function () {
                    var sHashAfter = currentHash();
                    var sRouteAfter = String(ModelStateRuntime.read(this, STATE_MODEL, "/currentRouteName", "") || "");
                    this._iDetailCloseFallbackTimer = null;
                    if (sHashAfter !== sHashBefore || sRouteAfter !== sRouteBefore || !NavigationContracts.isDetailRoute(sRouteAfter)) {
                        return;
                    }
                    this.onCloseDetail();
                }.bind(this), 0);
            }.bind(this);
            this._oDetailCloseButtonDom = oViewDom;
            toggleCloseFallbackListeners(oViewDom, this._fnDetailCloseBrowserClick, true);
        },

        _unbindDetailCloseButtonFallback: function () {
            if (this._oDetailCloseButtonDom && this._fnDetailCloseBrowserClick) {
                toggleCloseFallbackListeners(this._oDetailCloseButtonDom, this._fnDetailCloseBrowserClick, false);
            }
            this._oDetailCloseButtonDom = null;
            this._fnDetailCloseBrowserClick = null;
        },

        _replayInitialDetailRouteIfNeeded: function () {
            var oParsedRoute = parseInitialDetailHash();
            var sCurrentRouteName = String(ModelStateRuntime.read(this, STATE_MODEL, "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
            var sLoadedRootId = String(ModelStateRuntime.read(this, SELECTED_MODEL, "/root/id", "") || "").trim();

            if (!oParsedRoute || !oParsedRoute.id || sLoadedRootId || !NavigationContracts.isDetailRoute(sCurrentRouteName) || this._bDetailInitialRouteHandled) {
                return;
            }
            this._bDetailInitialRouteHandled = true;
            this._onDetailMatched({
                getParameter: function (sName) {
                    if (sName === "arguments") {
                        return {
                            id: oParsedRoute.id,
                            layout: oParsedRoute.layout
                        };
                    }
                    if (sName === "name") {
                        return oParsedRoute.layout ? NavigationContracts.ROUTES.DETAIL_LAYOUT : NavigationContracts.ROUTES.DETAIL;
                    }
                    return null;
                }
            });
        },

        _onDetailRouteLeave: function () {
            var oOwner = this.getOwnerComponent && this.getOwnerComponent();
            var oStateModel = ControllerModelRuntime.state(this);
            if (oOwner && typeof oOwner._stopLockScopedManagers === "function") {
                oOwner._stopLockScopedManagers();
            }
            if (oOwner && typeof oOwner._releaseActiveLockOnLeave === "function") {
                oOwner._releaseActiveLockOnLeave(oStateModel, this.getModel && this.getModel());
            }
            if (this._mLazyDialogs) {
                Object.keys(this._mLazyDialogs).forEach(function (sKey) {
                    var oDialog = this._mLazyDialogs[sKey];
                    if (oDialog && typeof oDialog.close === "function") {
                        oDialog.close();
                    }
                }, this);
            }
            this._iAttachmentDropZoneBindTimer = SchedulingRuntime.clearTimer(this._iAttachmentDropZoneBindTimer);
            this._iDetailCloseFallbackTimer = SchedulingRuntime.clearTimer(this._iDetailCloseFallbackTimer);
            this._clearLocationValueHelpSearchTimer();
            this._iLocationVhTableSyncTimer = SchedulingRuntime.clearTimer(this._iLocationVhTableSyncTimer);
        }
    };
});
