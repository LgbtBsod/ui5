sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/InFlightRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailService",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailPageFlow",
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailViewStateFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerRouteRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailAttachmentViewState",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/StatusChipClassRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SemanticDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ModelContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (
    ControllerResourceCleanup,
    InFlightRegistry,
    DetailService,
    DetailPageFlow,
    DetailViewStateFactory,
    ControllerRouteRuntime,
    StatePaths,
    ControllerModelRuntime,
    ControllerViewStateRuntime,
    SchedulingRuntime,
    DetailAttachmentViewState,
    StatusChipClassRuntime,
    SemanticDomRuntime,
    NavigationContracts,
    ModelContracts,
    CreateSentinel
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;

    function getBundleText(oController, sKey) {
        var oI18nModel = oController.getModel && oController.getModel("i18n");
        var oBundle = oI18nModel && oI18nModel.getResourceBundle && oI18nModel.getResourceBundle();
        return oBundle && oBundle.getText ? oBundle.getText(sKey) : "";
    }

    function syncSemanticRegions(oController) {
        SemanticDomRuntime.syncControllerTarget(oController, "detailControlActionRow", {
            role: "region",
            "aria-label": getBundleText(oController, "detailActionRailAriaLabel")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "detailHeroStatsRegion", {
            role: "region",
            "aria-label": getBundleText(oController, "detailStatsAriaLabel")
        });
        SemanticDomRuntime.syncControllerTarget(oController, "detailSectionAnchorRail", {
            role: "navigation",
            "aria-label": getBundleText(oController, "sectionNavAriaLabel")
        });
    }

    function syncComputedEditFlags(oController) {
        var oStateModel = oController && oController._oStateValidationModel;
        var oSelectedModel = oController && oController._oSelectedModel;
        var oViewModel = ControllerModelRuntime.viewState(oController);
        var sActiveObjectId;
        var sSelectedRootId;
        var sMode;
        if (!oStateModel || !oViewModel) {
            return;
        }
        sMode = String(oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_EDIT_MODE) || "").trim().toUpperCase() || "READ";
        sActiveObjectId = String(oStateModel.getProperty("/activeObjectId") || "").trim();
        sSelectedRootId = String((oSelectedModel && oSelectedModel.getProperty && oSelectedModel.getProperty("/root/id")) || "").trim();
        oViewModel.setProperty("/isEditMode", sMode !== "READ");
        oViewModel.setProperty("/isCreateMode", sMode === "CREATE");
        oViewModel.setProperty("/hasPersistedObject",
            (!!sActiveObjectId && !CreateSentinel.isCreateId(sActiveObjectId)) ||
            (!!sSelectedRootId && !CreateSentinel.isCreateId(sSelectedRootId))
        );
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
                return;
            }
            /* D-03 FIX: sync view>/isEditMode and view>/isCreateMode computed properties.
             * Avoids scattered inline expression bindings across XML fragments. */
            if (sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                syncComputedEditFlags(oController);
            }
            if (sPath !== StatePaths.WORKFLOW_DETAIL_EDIT_MODE && sPath !== "/activeObjectId" && sPath !== "/selectedId") {
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
            DetailAttachmentViewState.sync(this);

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
            syncSemanticRegions(this);
            StatusChipClassRuntime.syncView(this);
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
            /* C-01 FIX: clear in-flight dedup registry to prevent stale promise
             * re-use when navigating between different detail objects in FCL.     */
            InFlightRegistry.clear();
            return DetailPageFlow.onMatched(this, oEvent, {
                applyLayoutState: this._applyLayoutState.bind(this),
                scheduleAttachmentDropZoneBind: this._scheduleAttachmentDropZoneBind.bind(this),
                validationSummaryPath: StatePaths.VALIDATION_SUMMARY
            });
        }
    };
});
