sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailAccessViewState",
    "PRODUCTION_CONTROL_CHECKLIST/controller/support/DetailInfoCardLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts"
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
    NavigationContracts
) {
    "use strict";

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
            attachmentCategoryKey: "GEN",
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
                { key: "datetime", title: fnText("dateTimeBlockLabel", "Date & Time"), pinned: true },
                { key: "observer", title: fnText("observerLabel", "Observer"), pinned: true },
                { key: "observed", title: fnText("observedLabel", "Observed"), pinned: true },
                { key: "equipment", title: fnText("equipmentLabel", "Equipment"), pinned: false },
                { key: "location", title: fnText("locationLabel", "Location"), pinned: false },
                { key: "lpc", title: fnText("lpcLabel", "LPC"), pinned: false },
                { key: "profession", title: fnText("professionLabel", "Profession"), pinned: false }
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

    return {
        onInit: function () {
            this._facade = new DetailFacade();
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
                { name: NavigationContracts.ROUTES.DETAIL_LAYOUT, handler: this._onDetailMatched }
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
            this._clearLocationValueHelpSearchTimer();
            this._iLocationVhTableSyncTimer = SchedulingRuntime.clearTimer(this._iLocationVhTableSyncTimer);
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
        },

        _replayInitialDetailRouteIfNeeded: function () {
            var oParsedRoute = parseInitialDetailHash();
            var sCurrentRouteName = String(ModelStateRuntime.read(this, "state", "/currentRouteName", NavigationContracts.ROUTES.SEARCH) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
            var sLoadedRootId = String(ModelStateRuntime.read(this, "selected", "/root/id", "") || "").trim();

            if (!oParsedRoute || !oParsedRoute.id || sLoadedRootId || sCurrentRouteName !== NavigationContracts.ROUTES.SEARCH) {
                return;
            }
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
        }
    };
});
