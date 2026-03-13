sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/ShellPaneContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/AppShellCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ReadinessTelemetryContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellPaneRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellViewportRuntime",
    "sap/ui/Device"
], function (ControllerResourceCleanup, ShellPaneContracts, AppShellCoordinator, ControllerModelRuntime, ModelStateRuntime, SchedulingRuntime, NavigationContracts, ReadinessTelemetryContracts, ReadinessTelemetryRuntime, ShellLayoutRuntime, ShellPaneRuntime, ShellViewportRuntime, Device) {
    "use strict";

    function markStartupReady() {
        if (typeof window === "undefined" || typeof window.__ui5MarkAppReady !== "function") {
            return;
        }
        window.__ui5MarkAppReady();
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
            this._ensureAppViewDefaults();
            this._bindLayoutSync();
            this._bindShellStateSync();
            this._bindShellPaneRouting();
            this._fnViewportResize = this._syncResponsiveViewport.bind(this);
            Device.resize.attachHandler(this._fnViewportResize);
            this._syncResponsiveViewport();
            this._syncShellState();
            ShellPaneRuntime.writePaneLoaded(this, ShellPaneContracts.PANES.SEARCH, true);
        },
        onAfterRendering: function () {
            var oOwner = this.getOwnerComponent && this.getOwnerComponent();
            if (oOwner && typeof oOwner.attachInteractionFxToApp === "function") {
                oOwner.attachInteractionFxToApp(this.getView().getDomRef());
            }
            this._syncStaticAreaScope();
            this._syncLayoutState();
            this._applyCompactDensityClass();
            this._applyInvertedBlockSchemeClass();
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
            if (this._oShellStateModel && this._fnShellStateChange) {
                this._oShellStateModel.detachPropertyChange(this._fnShellStateChange, this);
            }
            if (this._oShellLayoutModel && this._fnShellLayoutChange) {
                this._oShellLayoutModel.detachPropertyChange(this._fnShellLayoutChange, this);
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
            this._oShellStateModel = null;
            this._oShellLayoutModel = null;
            this._fnShellStateChange = null;
            this._fnShellLayoutChange = null;
            this._fnViewportResize = null;
            this._fnShellPaneRouteGuard = null;
            this._oRouter = null;
            this._fnLayoutSync = null;
            AppShellCoordinator.onExit(this);
        },
        _bindShellPaneRouting: function () {
            var oRouter = this.getRouter && this.getRouter();
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
            this._oLayoutBinding = oState.bindProperty("/layout");
            this._oLayoutBinding.attachChange(this._fnLayoutSync);
            this._oRouteNameBinding = oState.bindProperty("/currentRouteName");
            this._oRouteNameBinding.attachChange(this._fnLayoutSync);
        },
        _syncLayoutState: function () {
            var oStateModel = this._getStateModel();
            var sRouteName;
            var oTargetPane;
            var oLayout;
            ShellLayoutRuntime.syncLayoutState(this, oStateModel);
            sRouteName = String(ModelStateRuntime.readOnModel(
                oStateModel,
                "/currentRouteName",
                NavigationContracts.ROUTES.SEARCH
            ) || NavigationContracts.ROUTES.SEARCH).trim() || NavigationContracts.ROUTES.SEARCH;
            oTargetPane = ShellPaneRuntime.ensurePaneForRoute(this, sRouteName, NavigationContracts);
            ShellLayoutRuntime.syncLayoutState(this, oStateModel);
            oLayout = this.byId && this.byId("mainFcl");
            if (oLayout && oTargetPane && NavigationContracts.ROUTES.SEARCH !== sRouteName && typeof oLayout.toMidColumnPage === "function") {
                oLayout.toMidColumnPage(oTargetPane.getParent && oTargetPane.getParent() ? oTargetPane.getParent() : oTargetPane);
            }
        },
        _markStartupReady: function () {
            var oStateModel;
            var bAppReady;
            var bIsLoading;
            if (this._bStartupReadyMarked) {
                return;
            }
            oStateModel = this._getStateModel();
            bAppReady = !!ModelStateRuntime.readOnModel(oStateModel, "/readiness/app/ready", false);
            bIsLoading = !!ModelStateRuntime.readOnModel(oStateModel, "/isLoading", false);
            if (!bAppReady || bIsLoading) {
                return;
            }
            this._bStartupReadyMarked = true;
            ReadinessTelemetryRuntime.markControllerStage(this, ReadinessTelemetryContracts.STAGES.SHELL_READY, {
                route: String(ModelStateRuntime.readOnModel(oStateModel, "/currentRouteName", "") || "").trim()
            });
            if (typeof window !== "undefined" && typeof window.requestAnimationFrame === "function") {
                SchedulingRuntime.nextFrame(markStartupReady);
                return;
            }
            markStartupReady();
        },
        _bindShellStateSync: function () {
            var oState = this._getStateModel();
            var oLayout = this._getLayoutModel();
            if (oState && !this._fnShellStateChange) {
                this._fnShellStateChange = this._syncShellState.bind(this);
                this._oShellStateModel = oState;
                oState.attachPropertyChange(this._fnShellStateChange, this);
                this._oShellUserBinding = oState.bindProperty("/currentUser/fetchedAt");
                this._oShellUserBinding.attachChange(this._fnShellStateChange);
                this._oShellFrontendSourceBinding = oState.bindProperty("/frontendConfigSource");
                this._oShellFrontendSourceBinding.attachChange(this._fnShellStateChange);
            }
            if (oLayout && !this._fnShellLayoutChange) {
                this._fnShellLayoutChange = this._syncShellState.bind(this);
                this._oShellLayoutModel = oLayout;
                oLayout.attachPropertyChange(this._fnShellLayoutChange, this);
            }
        },
        _getStateModel: function () { return ControllerModelRuntime.state(this); },
        _getLayoutModel: function () { return ControllerModelRuntime.layout(this); },
        _getAppViewModel: function () { return ControllerModelRuntime.appView(this); },
        _syncResponsiveViewport: function () { ShellViewportRuntime.syncResponsiveViewport(this); }
    };
});
