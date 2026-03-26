sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailFacade",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPageFlow",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailViewStateFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/StatusChipClassRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SemanticDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (
    ControllerResourceCleanup,
    DetailFacade,
    DetailPageFlow,
    DetailViewStateFactory,
    ControllerRouteRuntime,
    StatePaths,
    ControllerModelRuntime,
    ControllerViewStateRuntime,
    SchedulingRuntime,
    ControllerCommandContextRuntime,
    DetailAttachmentViewState,
    StatusChipClassRuntime,
    SemanticDomRuntime,
    ControllerTextRuntime,
    NavigationContracts,
    ModelContracts,
    CreateSentinel,
    WorkflowContracts,
    ModelPathContracts
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var DETAIL_MODEL = MODELS.DETAIL;

    function syncSemanticRegions(oController) {
        SemanticDomRuntime.syncControllerTarget(oController, "detailControlActionRow", {
            role: "region",
            "aria-label": ControllerTextRuntime.getText(oController, "detailActionRailAriaLabel", [], "")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "detailHeroStatsRegion", {
            role: "region",
            "aria-label": ControllerTextRuntime.getText(oController, "detailStatsAriaLabel", [], "")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "detailSectionAnchorRail", {
            role: "navigation",
            "aria-label": ControllerTextRuntime.getText(oController, "sectionNavAriaLabel", [], "")
        });
    }

    function syncComputedEditFlags(oController) {
        var oStateModel = oController && oController._oStateValidationModel;
        var oViewModel = ControllerModelRuntime.viewState(oController);
        var sActiveObjectId;
        var sMode;
        if (!oStateModel || !oViewModel) {
            return;
        }
        sMode = WorkflowContracts.normalizeEditMode(oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_EDIT_MODE));
        sActiveObjectId = String(oStateModel.getProperty(ModelPathContracts.ACTIVE_OBJECT_ID) || "").trim();
        oViewModel.setProperty("/isEditMode", WorkflowContracts.isEditableMode(sMode));
        oViewModel.setProperty("/isCreateMode", sMode === WorkflowContracts.EDIT_MODES.CREATE);
        oViewModel.setProperty("/hasPersistedObject", !!sActiveObjectId && !CreateSentinel.isCreateId(sActiveObjectId));
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
                DetailAttachmentViewState.sync(oController);
                syncComputedEditFlags(oController);
                return;
            }
            if (sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                syncComputedEditFlags(oController);
            }
            if (sPath !== StatePaths.WORKFLOW_DETAIL_EDIT_MODE && sPath !== ModelPathContracts.ACTIVE_OBJECT_ID && sPath !== ModelPathContracts.SELECTED_ID) {
                return;
            }
            DetailAttachmentViewState.sync(oController);
            oViewModel = ControllerModelRuntime.viewState(oController);
            if (oViewModel) {
                ControllerViewStateRuntime.set(oController, "/deleteChecklistConfirmArmed", false);
            }
            syncComputedEditFlags(oController);
        };
        oController._oStateValidationModel.attachPropertyChange(oController._fnStateValidationChange, oController);
        syncComputedEditFlags(oController);
    }

    return {
        onInit: function () {
            this._detailService = new DetailFacade();
            this._mLazyDialogs = {};
            this._mDialogReturnFocus = {};
            this._iAttachmentDropZoneBindTimer = null;
            this._iDetailPhaseLoadTimer = null;
            this._iLocationVhSearchTimer = null;
            this._iLocationVhTableSyncTimer = null;
            this._oDetailScrollHost = null;
            this._fnDetailScrollSync = null;
            this._fnDetailResizeSync = null;
            this._oAdaptiveViewportResizeObserver = null;
            this._fnAdaptiveViewportSync = null;
            ControllerRouteRuntime.attachMatched(this, [
                { name: NavigationContracts.ROUTES.DETAIL, handler: this._onDetailMatched },
                { name: NavigationContracts.ROUTES.SEARCH, handler: this._onDetailRouteLeave },
                { name: NavigationContracts.ROUTES.ANALYTICS, handler: this._onDetailRouteLeave }
            ]);

            ControllerViewStateRuntime.initModel(this, DetailViewStateFactory.create.bind(null, this));
            DetailAttachmentViewState.sync(this);

            this._oDetailModel = ControllerModelRuntime.detail(this);
            if (this._oDetailModel && this._oDetailModel.attachPropertyChange) {
                this._oDetailModel.attachPropertyChange(this._onDetailModelChanged, this);
            }
            bindStateValidationModel(this);
        },

        onAfterRendering: function () {
            this._scheduleAttachmentDropZoneBind();
            this._bindDetailEditSwitchKeyboardFallback();
            this._bindAdaptiveDetailViewport();
            this._bindViewportPinnedControlRail();
            syncSemanticRegions(this);
            StatusChipClassRuntime.syncView(this);
        },

        onExit: function () {
            if (this._oDetailModel && this._oDetailModel.detachPropertyChange) {
                this._oDetailModel.detachPropertyChange(this._onDetailModelChanged, this);
            }
            if (this._oStateValidationModel && this._fnStateValidationChange && this._oStateValidationModel.detachPropertyChange) {
                this._oStateValidationModel.detachPropertyChange(this._fnStateValidationChange, this);
            }
            ControllerRouteRuntime.detachAllMatched(this);
            this._iAttachmentDropZoneBindTimer = SchedulingRuntime.clearTimer(this._iAttachmentDropZoneBindTimer);
            this._iDetailPhaseLoadTimer = SchedulingRuntime.clearTimer(this._iDetailPhaseLoadTimer);
            this._clearLocationValueHelpSearchTimer();
            this._iLocationVhTableSyncTimer = SchedulingRuntime.clearTimer(this._iLocationVhTableSyncTimer);
            this._unbindViewportPinnedControlRail();
            this._unbindAttachmentDropZone();
            ControllerResourceCleanup.destroyMapEntries(this._mLazyDialogs);
            this._oDetailModel = null;
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
            var sPath = sKind === "barrier" ? "/current/barriers" : "/current/checks";
            var oDetailModel = this.getModel && this.getModel(DETAIL_MODEL);
            var aRows = oDetailModel && oDetailModel.getProperty ? oDetailModel.getProperty(sPath) : [];
            return !Array.isArray(aRows) || aRows.length === 0;
        },

        formatSectionRowCount: function (sKind) {
            var sPath = sKind === "barrier" ? "/current/barriers" : "/current/checks";
            var oDetailModel = this.getModel && this.getModel(DETAIL_MODEL);
            var aRows = oDetailModel && oDetailModel.getProperty ? oDetailModel.getProperty(sPath) : [];
            return this.formatRowCount(Array.isArray(aRows) ? aRows : []);
        },

        _onDetailMatched: function (oEvent) {
            return Promise.resolve(DetailPageFlow.onMatched(this, oEvent, {
                applyLayoutState: this._applyLayoutState.bind(this),
                buildCommandContext: ControllerCommandContextRuntime.buildCtx.bind(null, this),
                scheduleAttachmentDropZoneBind: this._scheduleAttachmentDropZoneBind.bind(this),
                validationSummaryPath: StatePaths.VALIDATION_SUMMARY
            })).then(function (vResult) {
                syncComputedEditFlags(this);
                return vResult;
            }.bind(this));
        }
    };
});
