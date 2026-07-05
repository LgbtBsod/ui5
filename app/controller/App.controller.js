sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/Base.controller",
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerResourceCleanup",
    "PRODUCTION_CONTROL_CHECKLIST/controller/base/ControllerTextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/RouteModeCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/constants/NavigationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RuntimeOrchestrationContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ReadinessTelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellLayoutRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellPaneRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellViewportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ShellGlobalsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime",
    "sap/ui/Device",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ClipboardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LazyDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/RetryCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/controller/shared/ControllerReturnFocusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerState",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/navigation/WorkspaceRouteNavigation",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/UiDecisionDefaultHandlers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeDomRuntime",
    "sap/ui/core/Core",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/AppShellDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SemanticDomRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants"
], function (
    BaseController,
    ControllerResourceCleanup,
    ControllerTextRuntime,
    ModelStateRuntime,
    SchedulingRuntime,
    RouteModeCoordinator,
    NavigationContracts,
    ReadinessTelemetryContracts,
    ReadinessTelemetryRuntime,
    ShellLayoutRuntime,
    ShellPaneRuntime,
    ShellViewportRuntime,
    ShellGlobalsRuntime,
    ShellStateRuntime,
    Device,
    JsRuntime,
    StatePaths,
    ModelContracts,
    ActionContract,
    LoadCurrentUserUseCase,
    ClipboardRuntime,
    FocusRuntime,
    LazyDialogRuntime,
    RetryCoordinator,
    ControllerReturnFocusRuntime,
    FeedbackBannerState,
    FeedbackBannerRuntime,
    WorkspaceRouteNavigation,
    UiDecisionDefaultHandlers,
    ThemeDomRuntime,
    Core,
    AppShellDomRuntime,
    SemanticDomRuntime,
    MessageCodeConstants
) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var SHELL_MODEL = MODELS.SHELL;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var DETAIL_MESSAGE_CODES = MessageCodeConstants.DETAIL;
    var getText = ControllerTextRuntime.getText;
    var _RESIZE_END_DELAY_MS = 520;
    var _RESIZE_SAFETY_DELAY_MS = 1800;
    var SHELL_OVERLAY_FRAGMENTS = {
        help: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.ShellHelpPopover",
        settings: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.ShellSettingsPopover",
        user: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.ShellUserPopover"
    };

    function markStartupReady() {
        ShellGlobalsRuntime.markAppReady();
    }

    function resolveControllerView(oController) {
        return oController && typeof oController.getView === TYPE_FUNCTION ? oController.getView() : null;
    }

    function resolveControllerOwner(oController) {
        return oController && typeof oController.getOwnerComponent === TYPE_FUNCTION ? oController.getOwnerComponent() : null;
    }

    function resolveControllerModel(oController, sName, bOwnerFallback) {
        var oView = resolveControllerView(oController);
        var oOwner = bOwnerFallback === false ? null : resolveControllerOwner(oController);
        return (oView && typeof oView.getModel === TYPE_FUNCTION && oView.getModel(sName))
            || (oOwner && typeof oOwner.getModel === TYPE_FUNCTION && oOwner.getModel(sName))
            || null;
    }

    function openShellOverlayByKey(oController, oEvent, sKey) {
        var sFragment = SHELL_OVERLAY_FRAGMENTS[sKey];
        if (!sFragment) {
            return Promise.resolve();
        }
        return oController._openShellOverlay(oEvent, sKey, sFragment);
    }

    function refreshCurrentUser(oController) {
        var oState = resolveControllerModel(oController, MODELS.STATE, true);
        var oShellModel = resolveControllerModel(oController, MODELS.SHELL, true);
        var bAlreadyBusy = !!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_USER_REFRESH_BUSY, false);
        if (!oState || bAlreadyBusy) {
            return Promise.resolve(false);
        }
        if (oShellModel) {
            ModelStateRuntime.writeOnModel(oShellModel, MODEL_PATHS.SHELL_USER_REFRESH_BUSY, true);
        }
        return LoadCurrentUserUseCase.refresh({
            stateModel: oState
        }).then(function (oResult) {
            oController._syncShellState();
            oController._syncShellMetrics();
            if (oResult && oResult.ok) {
                UiDecisionDefaultHandlers.handlers.notifyShellRefreshSuccess({ controller: oController });
            }
            return !!(oResult && oResult.ok);
        }).catch(function (oError) {
                UiDecisionDefaultHandlers.handlers.notifyShellRefreshFailure({ controller: oController, error: oError });
            return false;
        }).finally(function () {
            if (oShellModel) {
                ModelStateRuntime.writeOnModel(oShellModel, MODEL_PATHS.SHELL_USER_REFRESH_BUSY, false);
            }
        });
    }

    function refreshSecurityToken(oModel) {
        if (!oModel || typeof oModel.refreshSecurityToken !== "function") {
            return Promise.reject(new Error(DETAIL_MESSAGE_CODES.TECHNICAL_ERROR));
        }
        return new Promise(function (resolve, reject) {
            oModel.refreshSecurityToken(function () { resolve(true); }, reject, true);
        });
    }

    function refreshShellUserContext(oController) {
        var oModel = resolveControllerModel(oController, undefined, true);
        function syncShellUi() {
            oController._syncShellState();
            oController._syncShellMetrics();
        }
        return refreshSecurityToken(oModel).then(function () {
            return refreshCurrentUser(oController);
        }).then(function (bRefreshed) {
            if (bRefreshed) {
                oController._closeShellOverlay("user");
            }
            syncShellUi();
            return bRefreshed;
        }).catch(function (oError) {
            UiDecisionDefaultHandlers.handlers.notifyShellRefreshFailure({ controller: oController, error: oError });
            syncShellUi();
            throw oError;
        });
    }

    function getBackgroundRuntime() {
        return ShellGlobalsRuntime.getBackgroundRuntime();
    }

    function getGlobalDomNodes() {
        var oNodes = ThemeDomRuntime.getNodes();
        return {
            root: oNodes.root || null,
            body: oNodes.body || null,
            container: oNodes.container || null
        };
    }

    function syncSemanticAttributes(oController) {
        SemanticDomRuntime.syncControllerTarget(oController, "mainContentHost", { role: "main" });
        SemanticDomRuntime.syncControllerTarget(oController, "feedbackCorrelationRegion", { role: "region" });
    }

    function initializeAppShell(oController) {
        var oApplied = oController.applyStoredTheme();
        var oState = resolveControllerModel(oController, MODELS.STATE, true);
        var oShell = resolveControllerModel(oController, MODELS.SHELL, true);
        var oModelPatch = {};

        if (oShell) {
            ModelStateRuntime.setManyOnModel(oShell, {
                [MODEL_PATHS.SHELL_THEME_MODE]: "morning",
                [MODEL_PATHS.SHELL_ANIMATION_ENABLED]: !oApplied || oApplied.animationEnabled !== false,
                [MODEL_PATHS.SHELL_LAYOUT]: ModelStateRuntime.readOnModel(oShell, MODEL_PATHS.SHELL_LAYOUT, NavigationContracts.LAYOUTS.ONE_COLUMN) || NavigationContracts.LAYOUTS.ONE_COLUMN
            });
        }
        if (oState) {
            oModelPatch[StatePaths.SELECTED_ID] = typeof ModelStateRuntime.readOnModel(oState, StatePaths.SELECTED_ID, undefined) === "undefined"
                ? null
                : ModelStateRuntime.readOnModel(oState, StatePaths.SELECTED_ID, null);
            ModelStateRuntime.setManyOnModel(oState, oModelPatch);
            if (!oController._oRouteModeCoordinator) {
                oController._oRouteModeCoordinator = new RouteModeCoordinator({
                    router: oController.getRouter(),
                    stateModel: oState
                });
                oController._oRouteModeCoordinator.start();
            }
        }
    }

    function teardownAppShell(oController) {
        if (oController && oController._oRouteModeCoordinator) {
            oController._oRouteModeCoordinator.stop();
            oController._oRouteModeCoordinator = null;
        }
    }

    return BaseController.extend("PRODUCTION_CONTROL_CHECKLIST.controller.App", {
        onInit: function () {
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
            initializeAppShell(this);
            this._fnViewportResize = this._syncResponsiveViewport.bind(this);
            Device.resize.attachHandler(this._fnViewportResize);
            this._syncResponsiveViewport();
            this._syncShellState();
        },

        onAfterRendering: function () {
            var oOwner = typeof this.getOwnerComponent === TYPE_FUNCTION && this.getOwnerComponent();
            if (oOwner && typeof oOwner.attachInteractionFxToApp === TYPE_FUNCTION) {
                oOwner.attachInteractionFxToApp(AppShellDomRuntime.resolveViewDom(this));
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
            teardownAppShell(this);
        },

        onToggleTheme: function () {
            this.toggleTheme(null);
            this._syncShellState();
        },

        onSelectThemeMode: function (oEvent) {
            var sMode = String(oEvent && oEvent.getParameter && oEvent.getParameter("key") || "").trim();
            if (!sMode) {
                return;
            }
            this.setThemeMode(sMode);
            this._syncShellState();
        },

        onOpenShellHelp: function (oEvent) { return openShellOverlayByKey(this, oEvent, "help"); },
        onOpenShellSettings: function (oEvent) { return openShellOverlayByKey(this, oEvent, "settings"); },
        onOpenShellAnalytics: function () { WorkspaceRouteNavigation.navigateToAnalytics(this); },
        onOpenShellUserMenu: function (oEvent) { return openShellOverlayByKey(this, oEvent, "user"); },

        onGlobalBannerRetry: function () {
            var oState = this._getStateModel();
            var sAction = String(FeedbackBannerRuntime.getBannerProperty(oState, "global", "retryAction") || "").trim();
            return RetryCoordinator.runRetry(this, sAction);
        },

        onCopyFeedbackCorrelationId: function () {
            var oState = this._getStateModel();
            var sCorrelationId = String(FeedbackBannerRuntime.getBannerProperty(oState, "global", "correlationId") || "").trim();
            if (!sCorrelationId) {
                return;
            }
            ClipboardRuntime.writeText(sCorrelationId).then(function (bCopied) {
                if (bCopied) {
                UiDecisionDefaultHandlers.handlers.notifyCorrelationCopied({ controller: this });
                }
            }.bind(this));
        },

        onToggleShellHints: function (oEvent) {
            var bState = !!(oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            ModelStateRuntime.write(this, SHELL_MODEL, MODEL_PATHS.SHELL_PERSONALIZATION_SHOW_HINTS, bState);
            this._syncShellState();
        },

        onToggleCompactDensity: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(this, SHELL_MODEL, MODEL_PATHS.SHELL_COMPACT_DENSITY, oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            this._applyCompactDensityClass();
            return bState;
        },

        onToggleThemeAnimation: function (oEvent) {
            var bState = ModelStateRuntime.writeBoolean(this, SHELL_MODEL, MODEL_PATHS.SHELL_ANIMATION_ENABLED, oEvent && oEvent.getParameter && oEvent.getParameter("state"));
            this.setThemeAnimationEnabled(bState);
            return bState;
        },

        onToggleInvertedBlockScheme: function () {
            var bCurrent = !!ModelStateRuntime.read(this, SHELL_MODEL, MODEL_PATHS.SHELL_INVERTED_BLOCK_SCHEME, false);
            var bState = ModelStateRuntime.writeBoolean(this, SHELL_MODEL, MODEL_PATHS.SHELL_INVERTED_BLOCK_SCHEME, !bCurrent);
            this._applyInvertedBlockSchemeClass();
            return bState;
        },

        formatGlobalBannerVisible: function (bVisible, sScope) {
            return !!bVisible && String(sScope || "global").toLowerCase() === "global";
        },

        formatGlobalBannerCorrelationVisible: function (sCorrelationId, sScope) {
            return !!String(sCorrelationId || "").trim() && String(sScope || "global").toLowerCase() === "global";
        },

        formatRouteBannerVisible: function (bVisible, sScope) {
            return !!bVisible && String(sScope || "").toLowerCase() === "route";
        },

        formatGlobalBannerType: function (sSeverity) { return FeedbackBannerState.toUi5MessageType(sSeverity); },

        onShellUserPrimaryAction: function () {
            var sActionKind = String(ModelStateRuntime.read(this, SHELL_MODEL, MODEL_PATHS.SHELL_USER_ACTION_KIND, "") || "").trim();
            if (ActionContract.normalizeShellUserAction(sActionKind) === ActionContract.SHELL_USER_ACTIONS.REFRESH_CONTEXT) {
                return this._refreshShellUserContext();
            }
            return undefined;
        },

        onRefreshShellUser: function () { return refreshCurrentUser(this); },
        _refreshShellUserContext: function () { return refreshShellUserContext(this); },

        _restoreTestUserDialogFocus: function () {
            var oTarget = this._oTestUserDialogReturnFocus || (this._mShellOverlayTriggers && this._mShellOverlayTriggers.user);
            FocusRuntime.focusSoon(oTarget);
            this._oTestUserDialogReturnFocus = null;
        },

        _ensureShellOverlay: function (sKey, sFragmentName) {
            return LazyDialogRuntime.ensureDialog(this, sKey, {
                fragmentName: sFragmentName,
                cacheProperty: "_mShellOverlays",
                afterOpen: function (oOverlay, oController) {
                    oController._focusShellOverlay(sKey, oOverlay);
                },
                afterClose: function (_oOverlay, oController) {
                    oController._restoreShellOverlayFocus(sKey);
                }
            });
        },

        _openShellOverlay: function (oEvent, sKey, sFragmentName) {
            var oSource = (oEvent && oEvent.getParameter && oEvent.getParameter("anchor")) ||
                (oEvent && oEvent.getSource && oEvent.getSource());
            ControllerReturnFocusRuntime.remember(this, sKey, oSource, {
                storeProperty: "_mShellOverlayTriggers"
            });
            return this._ensureShellOverlay(sKey, sFragmentName).then(function (oOverlay) {
                if (oOverlay && typeof oOverlay.openBy === "function" && oSource) {
                    oOverlay.openBy(oSource);
                    return;
                }
                if (oOverlay && typeof oOverlay.open === "function") {
                    oOverlay.open();
                }
            });
        },

        _closeShellOverlay: function (sKey, bSkipFocusRestore) {
            var oOverlay = this._mShellOverlays && this._mShellOverlays[sKey];
            if (bSkipFocusRestore) {
                ControllerReturnFocusRuntime.markSkipRestore(this, sKey, {
                    skipProperty: "_mShellOverlaySkipRestore"
                });
            }
            if (oOverlay && oOverlay.close) {
                oOverlay.close();
            }
        },

        _focusShellOverlay: function (sKey, oOverlay) {
            var sTargetId = this._mShellOverlayFocusTargets && this._mShellOverlayFocusTargets[sKey];
            var oTarget = sTargetId && this.byId && this.byId(sTargetId);
            if (!oTarget && oOverlay && typeof oOverlay.focus === "function") {
                oTarget = oOverlay;
            }
            FocusRuntime.focusSoon(oTarget);
        },

        _restoreShellOverlayFocus: function (sKey) {
            ControllerReturnFocusRuntime.restore(this, sKey, {
                storeProperty: "_mShellOverlayTriggers",
                skipProperty: "_mShellOverlaySkipRestore"
            });
        },

        onSkipToMainContent: function (oEvent) {
            AppShellDomRuntime.focusMainContent(this, oEvent);
        },

        _syncShellFlexAllocation: function () {
            AppShellDomRuntime.syncShellFlexItem(this);
        },

        _syncLayoutViewportGeometry: function () {
            var oLayout = this.byId("mainFcl");
            AppShellDomRuntime.beginResizeCycle(this, _RESIZE_END_DELAY_MS, _RESIZE_SAFETY_DELAY_MS, function () {
                AppShellDomRuntime.notifyBackgroundResize(getBackgroundRuntime(), "end");
            });
            AppShellDomRuntime.notifyBackgroundResize(getBackgroundRuntime(), "start");
            AppShellDomRuntime.scheduleInvalidate(this, oLayout);
            AppShellDomRuntime.scheduleResizeEnd(this, _RESIZE_END_DELAY_MS, _RESIZE_SAFETY_DELAY_MS, function () {
                AppShellDomRuntime.notifyBackgroundResize(getBackgroundRuntime(), "end");
            });
        },

        _syncStaticAreaScope: function () {
            var oStaticArea = Core && typeof Core.getStaticAreaRef === TYPE_FUNCTION ? Core.getStaticAreaRef() : null;
            if (!oStaticArea) {
                return;
            }
            ThemeDomRuntime.addClass([oStaticArea], "chkApp");
            ThemeDomRuntime.addClass([oStaticArea], "chkSkin");
        },

        _applyCompactDensityClass: function () {
            var bCompact = !!ModelStateRuntime.read(this, SHELL_MODEL, MODEL_PATHS.SHELL_COMPACT_DENSITY, false);
            ThemeDomRuntime.toggleClass(AppShellDomRuntime.getAppDomTargets(this), "appDensityCompact", bCompact);
        },

        _applyInvertedBlockSchemeClass: function () {
            var bEnabled = !!ModelStateRuntime.read(this, SHELL_MODEL, MODEL_PATHS.SHELL_INVERTED_BLOCK_SCHEME, false);
            ThemeDomRuntime.toggleClass(AppShellDomRuntime.getAppDomTargets(this), "appInvertedBlockScheme", bEnabled);
        },

        _syncShellMetrics: function () {
            AppShellDomRuntime.syncShellMetricVars(this, getGlobalDomNodes().root);
        },

        _syncSemanticAttributes: function () {
            syncSemanticAttributes(this);
        },

        _scheduleShellLayoutRefresh: function () {
            var oRuntimeState = AppShellDomRuntime.ensureDomRuntimeState(this);
            var that = this;
            oRuntimeState.shellRefreshRafId = SchedulingRuntime.restartFrame(oRuntimeState.shellRefreshRafId, function () {
                oRuntimeState.shellRefreshRafId = 0;
                SchedulingRuntime.nextFrame(function () {
                    that._syncShellFlexAllocation();
                    that._syncShellMetrics();
                    that._syncLayoutViewportGeometry();
                });
            });
        },

        _teardownAppDomRuntime: function () {
            AppShellDomRuntime.teardownResizeCycle(this);
            AppShellDomRuntime.notifyBackgroundResize(getBackgroundRuntime(), "end");
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
            var oShell = this._getShellModel();
            var oState = this._getStateModel();
            if (!oShell || !oState || this._oLayoutBinding) {
                return;
            }
            if (!this._fnLayoutSync) {
                this._fnLayoutSync = this._syncLayoutState.bind(this);
            }
            this._oLayoutBinding = oShell.bindProperty(ModelContracts.MODEL_PATHS.SHELL_LAYOUT);
            this._oLayoutBinding.attachChange(this._fnLayoutSync);
            this._oRouteNameBinding = oState.bindProperty(StatePaths.CURRENT_ROUTE_NAME);
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
                route: String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.CURRENT_ROUTE_NAME, "") || "").trim()
            });
            if (SchedulingRuntime.hasAnimationFrameSupport()) {
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
                this._oShellSelectedIdBinding = oState.bindProperty(StatePaths.SELECTED_ID);
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

        _ensureShellDefaults: function () {
            ShellStateRuntime.ensureShellDefaults(this);
        },

        _getStateModel: function () { return resolveControllerModel(this, MODELS.STATE, true); },
        _getShellModel: function () { return resolveControllerModel(this, MODELS.SHELL, true); },
        _getDefaultModel: function () { return resolveControllerModel(this, undefined, true); },

        _syncShellState: function () {
            ShellStateRuntime.syncShellState(this, {
                getText: getText
            });
        },

        _syncResponsiveViewport: function () { ShellViewportRuntime.syncResponsiveViewport(this); }
    });
});
