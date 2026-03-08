sap.ui.define([
    "checklist/app/controller/support/ControllerResourceCleanup",
    "checklist/app/service/framework/AppShellCoordinator",
    "checklist/app/service/framework/LayoutStateRuntime",
    "checklist/app/service/framework/RootIdRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/SchedulingRuntime",
    "checklist/app/util/ThemeDomRuntime",
    "sap/ui/Device"
], function (ControllerResourceCleanup, AppShellCoordinator, LayoutStateRuntime, RootIdRuntime, ModelStateRuntime, SchedulingRuntime, ThemeDomRuntime, Device) {
    "use strict";

    var PHONE_MAX_WIDTH = 720;
    var TABLET_MAX_WIDTH = 1080;

    function resolveMidColumnPageId(sRouteName) {
        if (sRouteName === "analytics") {
            return "analyticsPaneHost";
        }
        if (sRouteName === "accessDenied") {
            return "accessDeniedPaneHost";
        }
        return "detailPaneHost";
    }

    function syncMidColumnPage(oController, sRouteName) {
        var oLayout = oController.byId && oController.byId("mainFcl");
        var oTargetPage = oController.byId && oController.byId(resolveMidColumnPageId(sRouteName));
        var oCurrentPage;

        if (!oLayout || !oTargetPage || typeof oLayout.toMidColumnPage !== "function") {
            return;
        }
        oCurrentPage = oLayout.getCurrentMidColumnPage && oLayout.getCurrentMidColumnPage();
        if (oCurrentPage && oCurrentPage.getId && oCurrentPage.getId() === oTargetPage.getId()) {
            return;
        }
        oLayout.toMidColumnPage(oTargetPage);
    }

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
                notifications: "shellNotificationsList",
                settings: "shellSettingsAnimationSwitch",
                user: "shellUserRefreshButton"
            };
            this._oTestUserDialogReturnFocus = null;
            this._ensureAppViewDefaults();
            this._bindLayoutSync();
            this._bindShellStateSync();
            this._fnViewportResize = this._syncResponsiveViewport.bind(this);
            Device.resize.attachHandler(this._fnViewportResize);
            this._syncResponsiveViewport();
            this._syncShellState();
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
            if (this._oShellStateModel && this._fnShellStateChange) {
                this._oShellStateModel.detachPropertyChange(this._fnShellStateChange, this);
            }
            if (this._oShellLayoutModel && this._fnShellLayoutChange) {
                this._oShellLayoutModel.detachPropertyChange(this._fnShellLayoutChange, this);
            }
            if (this._fnViewportResize) {
                Device.resize.detachHandler(this._fnViewportResize);
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
            this._fnLayoutSync = null;
            AppShellCoordinator.onExit(this);
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
            var oState = this._getStateModel();
            var sLayoutRaw = ModelStateRuntime.read(this, "state", "/layout", "OneColumn");
            var sLayout = LayoutStateRuntime.normalizeLayout(sLayoutRaw);
            var sRouteName = String(ModelStateRuntime.read(this, "state", "/currentRouteName", "search") || "search").trim() || "search";
            var sSelectedId = RootIdRuntime.resolveFromStateModel(oState);
            var bSingle = sLayout === "OneColumn";
            var bDetailOnly = sLayout === "MidColumnFullScreen";
            var oRoot = this.getView && this.getView().getDomRef && this.getView().getDomRef();
            var oClassHost = (oRoot && oRoot.querySelector && oRoot.querySelector(".chkSkin")) || oRoot;
            var oLayout = this.byId && this.byId("mainFcl");
            if (!sSelectedId && sLayout !== "OneColumn" && sRouteName !== "analytics") {
                sLayout = "OneColumn";
                bSingle = true;
                bDetailOnly = false;
            }
            if (oClassHost && oClassHost.classList) {
                ThemeDomRuntime.toggleClass([oClassHost], "appLayoutSingle", bSingle);
                ThemeDomRuntime.toggleClass([oClassHost], "appLayoutSplit", !bSingle && !bDetailOnly);
                ThemeDomRuntime.toggleClass([oClassHost], "appLayoutDetailOnly", bDetailOnly);
            }
            if (oLayout && typeof oLayout.getLayout === "function" && typeof oLayout.setLayout === "function" && oLayout.getLayout() !== sLayout) {
                oLayout.setLayout(sLayout);
            }
            syncMidColumnPage(this, sRouteName);
            if (oState && oState.setProperty && sLayoutRaw !== sLayout) {
                ModelStateRuntime.writeOnModel(oState, "/layout", sLayout);
            }
        },

        _markStartupReady: function () {
            if (this._bStartupReadyMarked) {
                return;
            }
            this._bStartupReadyMarked = true;
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
            }
            if (oLayout && !this._fnShellLayoutChange) {
                this._fnShellLayoutChange = this._syncShellState.bind(this);
                this._oShellLayoutModel = oLayout;
                oLayout.attachPropertyChange(this._fnShellLayoutChange, this);
            }
        },

        _getStateModel: function () {
            var oView = this.getView && this.getView();
            var oOwner = this.getOwnerComponent && this.getOwnerComponent();
            return (oView && oView.getModel && oView.getModel("state")) ||
                (oOwner && oOwner.getModel && oOwner.getModel("state")) ||
                null;
        },

        _getLayoutModel: function () {
            var oView = this.getView && this.getView();
            var oOwner = this.getOwnerComponent && this.getOwnerComponent();
            return (oView && oView.getModel && oView.getModel("layout")) ||
                (oOwner && oOwner.getModel && oOwner.getModel("layout")) ||
                null;
        },

        _getAppViewModel: function () {
            var oView = this.getView && this.getView();
            return oView && oView.getModel && oView.getModel("appView");
        },

        _syncResponsiveViewport: function () {
            var iWidth = Math.max(window.innerWidth || 0, document.documentElement && document.documentElement.clientWidth || 0, 0);
            var bPhone = iWidth > 0 && iWidth <= PHONE_MAX_WIDTH;
            var bTablet = iWidth > PHONE_MAX_WIDTH && iWidth <= TABLET_MAX_WIDTH;
            ModelStateRuntime.write(this, "appView", "/isPhoneViewport", bPhone);
            ModelStateRuntime.write(this, "appView", "/isTabletViewport", bTablet);
            ModelStateRuntime.write(this, "appView", "/viewportWidth", iWidth);
            this._syncShellMetrics();
            this._syncLayoutViewportGeometry();
        }
    };
});
