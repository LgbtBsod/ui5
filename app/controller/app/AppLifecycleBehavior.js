sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/AppShellCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ReadinessTelemetryConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellPaneRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ShellGlobalsRuntime",
    "sap/ui/Device",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ControllerResourceCleanup, AppShellCoordinator, ControllerModelRuntime, ModelStateRuntime, SchedulingRuntime, NavigationContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, ShellLayoutRuntime, ShellPaneRuntime, ShellViewportRuntime, ShellGlobalsRuntime, Device, JsRuntime, ModelPathContracts, StatePaths, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;

    function markStartupReady() {
        ShellGlobalsRuntime.markAppReady();
    }

    return {
        onInit: function () {
            AppShellCoordinator.onInit(this);
            this._mShellOverlays = {};
            this._mShellOverlayTriggers = {};
            this._mShellOverlaySkipRestore = {};
            this._mShellOverlayFocusTargets = {
                settings: "shellSettingsAnimationSwitch",
                user: "shellUserRefreshButton"
            };
            this._oTestUserDialogReturnFocus = null;
            this._ensureShellDefaults();
            this._bindLayoutSync();
            this._bindShellStateSync();
            this._bindShellPaneRouting();
            this._fnViewportResize = this._syncResponsiveViewport.bind(this);
            Device.resize.attachHandler(this._fnViewportResize);
            this._syncResponsiveViewport();
            this._syncShellState();
        },
        onAfterRendering: function () {
            var oOwner = typeof this.getOwnerComponent === TYPE_FUNCTION && this.getOwnerComponent();
            if (oOwner && typeof oOwner.attachInteractionFxToApp === TYPE_FUNCTION) {
                oOwner.attachInteractionFxToApp(this.getView().getDomRef());
            }
            this._syncStaticAreaScope();
            this._syncSemanticAttributes();
            this._syncLayoutState();
            this._applyCompactDensityClass();
            this._applyInvertedBlockSchemeClass();
            this._syncShellState();
            this._syncShellMetrics();
            this._syncShellFlexAllocation();
            this._syncLayoutViewportGeometry();
            this._markStartupReady();
        },
        onExit: function () {
            if (this._oLayoutBinding) {
                this._oLayoutBinding = ControllerResourceCleanup.destroyBinding(this._oLayoutBinding, this._fnLayoutSync);
            }
            if (this._oRouteNameBinding) {
                this._oRouteNameBinding = ControllerResourceCleanup.destroyBinding(this._oRouteNameBinding, this._fnLayoutSync);
            }
            if (this._oShellUserBinding) {
                this._oShellUserBinding = ControllerResourceCleanup.destroyBinding(this._oShellUserBinding, this._fnShellStateChange);
            }
            if (this._oShellFrontendSourceBinding) {
                this._oShellFrontendSourceBinding = ControllerResourceCleanup.destroyBinding(this._oShellFrontendSourceBinding, this._fnShellStateChange);
            }
            if (this._oShellSelectedIdBinding) {
                this._oShellSelectedIdBinding = ControllerResourceCleanup.destroyBinding(this._oShellSelectedIdBinding, this._fnShellStateChange);
            }
            if (this._oShellRouteBinding) {
                this._oShellRouteBinding = ControllerResourceCleanup.destroyBinding(this._oShellRouteBinding, this._fnShellStateChange);
            }
            if (this._oShellHintsBinding) {
                this._oShellHintsBinding = ControllerResourceCleanup.destroyBinding(this._oShellHintsBinding, this._fnShellLayoutChange);
            }
            if (this._oShellPhoneViewportBinding) {
                this._oShellPhoneViewportBinding = ControllerResourceCleanup.destroyBinding(this._oShellPhoneViewportBinding, this._fnShellViewportChange);
            }
            if (this._oShellTabletViewportBinding) {
                this._oShellTabletViewportBinding = ControllerResourceCleanup.destroyBinding(this._oShellTabletViewportBinding, this._fnShellViewportChange);
            }
            if (this._fnViewportResize) {
                Device.resize.detachHandler(this._fnViewportResize);
            }
            if (this._oRouter && this._fnShellPaneRouteGuard && this._oRouter.detachBeforeRouteMatched) {
                this._oRouter.detachBeforeRouteMatched(this._fnShellPaneRouteGuard, this);
            }
            ControllerResourceCleanup.destroyMapEntries(this._mShellOverlays);
            this._mShellOverlays = null;
            this._mShellOverlayTriggers = null;
            this._mShellOverlaySkipRestore = null;
            this._mShellOverlayFocusTargets = null;
            this._oTestUserDialogReturnFocus = null;
            this._fnShellStateChange = null;
            this._fnShellLayoutChange = null;
            this._fnShellViewportChange = null;
            this._fnViewportResize = null;
            this._fnShellPaneRouteGuard = null;
            this._oRouter = null;
            this._fnLayoutSync = null;
            this._bStartupReadyMarked = false;
            this._bShellStatePostStartupSyncScheduled = false;
            if (typeof this._teardownAppDomRuntime === TYPE_FUNCTION) {
                this._teardownAppDomRuntime();
            }
            AppShellCoordinator.onExit(this);
        },
        _bindShellPaneRouting: function () {
            var oRouter = typeof this.getRouter === TYPE_FUNCTION && this.getRouter();
            if (!oRouter || !oRouter.attachBeforeRouteMatched || this._fnShellPaneRouteGuard) {
                return;
            }
            this._oRouter = oRouter;
            this._fnShellPaneRouteGuard = function (oEvent) {
                var sRouteName = String(oEvent && oEvent.getParameter && oEvent.getParameter("name") || "").trim();
                if (!sRouteName) {
                    return;
                }
                ShellPaneRuntime.ensurePaneForRoute(this, sRouteName, NavigationContracts);
            }.bind(this);
            oRouter.attachBeforeRouteMatched(this._fnShellPaneRouteGuard, this);
        },
        _bindLayoutSync: function () {
            var oState = this._getStateModel();
            if (!oState || this._oLayoutBinding) {
                return;
            }
            if (!this._fnLayoutSync) {
                this._fnLayoutSync = this._syncLayoutState.bind(this);
            }
            this._oLayoutBinding = oState.bindProperty(ModelPathContracts.LAYOUT);
            this._oLayoutBinding.attachChange(this._fnLayoutSync);
            this._oRouteNameBinding = oState.bindProperty(ModelPathContracts.CURRENT_ROUTE_NAME);
            this._oRouteNameBinding.attachChange(this._fnLayoutSync);
        },
        _syncLayoutState: function () {
            var oStateModel = this._getStateModel();
            ShellLayoutRuntime.syncLayoutState(this, oStateModel);
        },
        _markStartupReady: function () {
            var oStateModel;
            var bAppReady;
            var bIsLoading;
            if (this._bStartupReadyMarked) {
                return;
            }
            oStateModel = this._getStateModel();
            bAppReady = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.READINESS_APP_READY, false);
            bIsLoading = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.IS_LOADING, false);
            if (!bAppReady || bIsLoading) {
                return;
            }
            this._bStartupReadyMarked = true;
            ReadinessTelemetryRuntime.markControllerStage(this, ReadinessTelemetryContracts.STAGES.SHELL_READY, {
                route: String(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.CURRENT_ROUTE_NAME, "") || "").trim()
            });
            if (typeof window !== JsRuntime.TYPEOF.UNDEFINED && typeof window.requestAnimationFrame === TYPE_FUNCTION) {
                SchedulingRuntime.nextFrame(function () {
                    if (!this._bShellStatePostStartupSyncScheduled && typeof this._syncShellState === TYPE_FUNCTION) {
                        this._bShellStatePostStartupSyncScheduled = true;
                        this._syncShellState();
                    }
                    markStartupReady();
                }.bind(this));
                return;
            }
            if (!this._bShellStatePostStartupSyncScheduled && typeof this._syncShellState === TYPE_FUNCTION) {
                this._bShellStatePostStartupSyncScheduled = true;
                this._syncShellState();
            }
            markStartupReady();
        },
        _bindShellStateSync: function () {
            var oState = this._getStateModel();
            var oShell = this._getShellModel();
            if (oState && !this._fnShellStateChange) {
                this._fnShellStateChange = this._syncShellState.bind(this);
                this._oShellUserBinding = oState.bindProperty("/currentUser");
                this._oShellUserBinding.attachChange(this._fnShellStateChange);
                this._oShellFrontendSourceBinding = oState.bindProperty("/frontendConfigSource");
                this._oShellFrontendSourceBinding.attachChange(this._fnShellStateChange);
                this._oShellSelectedIdBinding = oState.bindProperty(ModelPathContracts.SELECTED_ID);
                this._oShellSelectedIdBinding.attachChange(this._fnShellStateChange);
                this._oShellRouteBinding = oState.bindProperty("/currentRouteName");
                this._oShellRouteBinding.attachChange(this._fnShellStateChange);
            }
            if (oShell && !this._fnShellLayoutChange) {
                this._fnShellLayoutChange = this._syncShellState.bind(this);
                this._oShellHintsBinding = oShell.bindProperty("/personalization/showHints");
                this._oShellHintsBinding.attachChange(this._fnShellLayoutChange);
            }
            if (oShell && !this._fnShellViewportChange) {
                this._fnShellViewportChange = this._syncShellState.bind(this);
                this._oShellPhoneViewportBinding = oShell.bindProperty("/isPhoneViewport");
                this._oShellPhoneViewportBinding.attachChange(this._fnShellViewportChange);
                this._oShellTabletViewportBinding = oShell.bindProperty("/isTabletViewport");
                this._oShellTabletViewportBinding.attachChange(this._fnShellViewportChange);
            }
        },
        _getStateModel: function () { return ControllerModelRuntime.state(this) || this.getModel(MODELS.STATE); },
        _getShellModel: function () { return ControllerModelRuntime.shell(this); },
        _syncResponsiveViewport: function () { ShellViewportRuntime.syncResponsiveViewport(this); }
    };
});
