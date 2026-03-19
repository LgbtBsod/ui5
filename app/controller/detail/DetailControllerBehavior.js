sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailService",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPageFlow",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailViewStateFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts"
], function (
    ControllerResourceCleanup,
    DetailService,
    DetailPageFlow,
    DetailViewStateFactory,
    ControllerRouteRuntime,
    StatePaths,
    ControllerModelRuntime,
    ControllerViewStateRuntime,
    SchedulingRuntime,
    NavigationContracts,
    ModelContracts
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;

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
            this._detailService = new DetailService(this);
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

            ControllerViewStateRuntime.initModel(this, DetailViewStateFactory.create.bind(null, this));

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

        _onDetailRouteLeave: function () {
            DetailPageFlow.onRouteLeave(this);
        },

        isDetailSectionBusy: function (sKind) {
            return sKind === "barrier"
                ? !!ControllerViewStateRuntime.get(this, "/barriersBusy", false)
                : !!ControllerViewStateRuntime.get(this, "/checksBusy", false);
        },

        isDetailSectionEmpty: function (sKind) {
            var sPath = sKind === "barrier" ? "/barriers" : "/checks";
            var oSelectedModel = this.getModel && this.getModel(SELECTED_MODEL);
            var aRows = oSelectedModel && oSelectedModel.getProperty ? oSelectedModel.getProperty(sPath) : [];
            return !Array.isArray(aRows) || aRows.length === 0;
        },

        formatSectionRowCount: function (sKind) {
            var sPath = sKind === "barrier" ? "/barriers" : "/checks";
            var oSelectedModel = this.getModel && this.getModel(SELECTED_MODEL);
            var aRows = oSelectedModel && oSelectedModel.getProperty ? oSelectedModel.getProperty(sPath) : [];
            return this.formatRowCount(Array.isArray(aRows) ? aRows : []);
        },

        _onDetailMatched: function (oEvent) {
            return DetailPageFlow.onMatched(this, oEvent, {
                applyLayoutState: this._applyLayoutState.bind(this),
                scheduleAttachmentDropZoneBind: this._scheduleAttachmentDropZoneBind.bind(this),
                validationSummaryPath: StatePaths.VALIDATION_SUMMARY
            });
        }
    };
});
