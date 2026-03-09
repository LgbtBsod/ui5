sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCrossTabRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCoordinatorRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentListenerRuntime"
], function (ModelStateRuntime, FeedbackBannerRuntime, ComponentActionRuntime, ComponentBootRuntime, ComponentCrossTabRuntime, ComponentCoordinatorRuntime, ComponentListenerRuntime) {
    "use strict";

    function reuseJsonModel(oExistingModel, fnCreateModel) {
        var oModel = oExistingModel || fnCreateModel();
        var oSeedModel;

        if (oExistingModel && typeof oExistingModel.setData === "function") {
            oSeedModel = fnCreateModel();
            oExistingModel.setData(oSeedModel && oSeedModel.getData ? oSeedModel.getData() : {}, false);
        }

        return oModel;
    }

    function buildActionValidators(ActionContract) {
        return ComponentActionRuntime.buildActionValidators(ActionContract);
    }

    function registerDefaultHandlers(mOptions) {
        return ComponentActionRuntime.registerDefaultHandlers(mOptions);
    }

    function resolveCorrelationId(oError, FeedbackPolicy) {
        var oNormalizedError = FeedbackPolicy && FeedbackPolicy.normalize ? FeedbackPolicy.normalize(oError || {}) : null;
        var oParams = oNormalizedError && oNormalizedError.params;
        return String(
            (oParams && (oParams.correlationId || oParams.correlation_id || oParams.requestId || oParams.request_id)) ||
            (oError && (oError.correlationId || oError.correlation_id || oError.requestId || oError.request_id)) ||
            ""
        ).trim();
    }

    function isSessionExpiredError(oError) {
        var iStatus = Number((oError && (oError.statusCode || oError.status)) || 0);
        var sCode = String((oError && oError.code) || "").toUpperCase();
        var sMessage = String((oError && oError.message) || "").toUpperCase();
        if (iStatus === 401 || iStatus === 403) {
            return true;
        }
        return sCode === "SESSION_UNAVAILABLE" || sCode === "AUTH_REQUIRED" || /SESSION|AUTH|CSRF/.test(sMessage);
    }

    function createFeedbackRuntime(oOptions) {
        var oStateModel = oOptions.stateModel;
        var FeedbackPolicy = oOptions.feedbackPolicy;
        var fnBundleText = oOptions.bundleText || function (sKey) {
            return sKey;
        };

        function setGlobalBanner(mBannerInput) {
            var mInput = mBannerInput || {};
            FeedbackBannerRuntime.setBanner(oStateModel, "global", mInput, {
                resolveText: fnBundleText
            });
        }

        function clearGlobalBanner() {
            FeedbackBannerRuntime.clearBanner(oStateModel, "global");
        }

        return {
            resolveCorrelationId: function (oError) {
                return resolveCorrelationId(oError, FeedbackPolicy);
            },
            isSessionExpiredError: isSessionExpiredError,
            setGlobalBanner: setGlobalBanner,
            clearGlobalBanner: clearGlobalBanner
        };
    }

    function createBundleText(component) {
        return ComponentActionRuntime.createBundleText(component);
    }

    function createApplyFacadeResult(mOptions) {
        return ComponentActionRuntime.createApplyFacadeResult(mOptions);
    }

    function queuePendingNavigationIntent(oStateModel, StatePaths, oRouteEvent) {
        ComponentActionRuntime.queuePendingNavigationIntent(oStateModel, StatePaths, oRouteEvent);
    }

    function clearPendingNavigationIntent(oStateModel, StatePaths) {
        ComponentActionRuntime.clearPendingNavigationIntent(oStateModel, StatePaths);
    }

    function resumePendingNavigationIntent(component, oStateModel, StatePaths) {
        return ComponentActionRuntime.resumePendingNavigationIntent(component, oStateModel, StatePaths);
    }

    function runBootSequence(mOptions) {
        return ComponentBootRuntime.runBootSequence(mOptions);
    }

    function attachCrossTabRuntime(mOptions) {
        return ComponentCrossTabRuntime.attachCrossTabRuntime(mOptions);
    }

    function attachInitListeners(mOptions) {
        return ComponentListenerRuntime.attachInitListeners(mOptions);
    }

    function attachLockRuntime(mOptions) {
        return ComponentCoordinatorRuntime.attachLockRuntime(mOptions);
    }

    function attachManagerRuntime(mOptions) {
        return ComponentCoordinatorRuntime.attachManagerRuntime(mOptions);
    }

    function runInit(aInitArgs, mDeps) {
        var UIComponent = mDeps.UIComponent;
        var ModelFactory = mDeps.ModelFactory;
        var SmartSearchAdapter = mDeps.SmartSearchAdapter;
        var Managers = mDeps.Managers || {};
        var HeartbeatManager = Managers.HeartbeatManager || mDeps.HeartbeatManager;
        var GCDManager = Managers.GCDManager || mDeps.GCDManager;
        var ActivityMonitor = Managers.ActivityMonitor || mDeps.ActivityMonitor;
        var AutoSaveCoordinator = Managers.AutoSaveCoordinator || mDeps.AutoSaveCoordinator;
        var LockStatusMonitor = Managers.LockStatusMonitor || mDeps.LockStatusMonitor;
        var JSONModel = mDeps.JSONModel;
        var FlowCoordinator = mDeps.FlowCoordinator;
        var DeltaPayloadBuilder = mDeps.DeltaPayloadBuilder;
        var GatewayBackendService = mDeps.GatewayBackendService;
        var SettingsManager = Managers.SettingsManager || mDeps.SettingsManager;
        var DebugLogger = mDeps.DebugLogger;
        var RuntimeTimerSanitizer = mDeps.RuntimeTimerSanitizer;
        var TimeConfigService = mDeps.TimeConfigService;
        var ApplyRuntimeSettingsUseCase = mDeps.ApplyRuntimeSettingsUseCase;
        var EnsureDictLoadedUseCase = mDeps.EnsureDictLoadedUseCase;
        var BootstrapAppUseCase = mDeps.BootstrapAppUseCase;
        var DiagnosticsUseCase = mDeps.DiagnosticsUseCase;
        var CtxFactory = mDeps.CtxFactory;
        var EffectApplier = mDeps.EffectApplier;
        var FeedbackPolicy = mDeps.FeedbackPolicy;
        var ComponentInitSaveGuardSupport = mDeps.ComponentInitSaveGuardSupport;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var TelemetryRuntime = mDeps.TelemetryRuntime;
        var LayoutStateRuntime = mDeps.LayoutStateRuntime;
        var StatePaths = mDeps.StatePaths;
        var DetailFacade = mDeps.DetailFacade;
        var ActionDispatcher = mDeps.ActionDispatcher;
        var ActionContract = mDeps.ActionContract;
        var ODataModel = mDeps.ODataModel;
        var WorkflowTelemetry = mDeps.WorkflowTelemetry;
        var CreateSentinel = mDeps.CreateSentinel;
        var Device = mDeps.Device;
        var InteractionFX = mDeps.InteractionFX;
        var ThemeRuntime = mDeps.ThemeRuntime;

            UIComponent.prototype.init.apply(this, aInitArgs || []);
            this._startupPerf = this._startupPerf || {
                t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
                firstRouteReadyLogged: false,
                analyticsStartedLogged: false
            };
            ThemeRuntime.syncDocumentRootClasses();
            var sConfiguredMode = this.getManifestEntry("/sap.ui5/config/backendMode") || "real";
            var sUiContractVersion = this.getManifestEntry("/sap.ui5/config/uiContractVersion") || "1.0.0";
            var sMainServiceUri = this.getManifestEntry("/sap.app/dataSources/mainService/uri") || "/sap/opu/odata/sap/Z_UI5_SRV/";
            var oDataModel = reuseJsonModel(this.getModel("data"), ModelFactory.createDataModel);
            var oMplModel = reuseJsonModel(this.getModel("mpl"), ModelFactory.createMplModel);
            var oStateModel = reuseJsonModel(this.getModel("state"), ModelFactory.createStateModel);
            var oUiStateModel = reuseJsonModel(this.getModel("uiState"), ModelFactory.createUiStateModel);
            var oViewModel = reuseJsonModel(this.getModel("view"), ModelFactory.createViewModel);
            var oSelectedModel = reuseJsonModel(this.getModel("selected"), function () { return new JSONModel({}); });
            var oMasterDataModel = reuseJsonModel(this.getModel("masterData"), ModelFactory.createMasterDataModel);
            var oDeviceModel = new JSONModel(Device);
            var oMainServiceModel = this.getModel("mainService") || new ODataModel(sMainServiceUri, {
                useBatch: true,
                tokenHandling: true,
                defaultBindingMode: "TwoWay",
                defaultCountMode: "Inline",
                refreshAfterChange: false
            });
            oMainServiceModel.setDeferredGroups(["changes", "autosave", "saveFlow", "locks"]);
            oMainServiceModel.setChangeGroups({
                "*": {
                    groupId: "changes",
                    changeSetId: "ChecklistSave",
                    single: false
                },
                "LockAcquireType": { groupId: "locks", single: true },
                "LockHeartbeatType": { groupId: "locks", single: true },
                "LockReleaseType": { groupId: "locks", single: true }
            });
            this.setModel(oMainServiceModel, "mainService");
            this.setModel(oMainServiceModel);
            GatewayBackendService.setModel(oMainServiceModel, { serviceUrl: sMainServiceUri });
            var fnBundleText = createBundleText(this);
            var oFeedbackRuntime = createFeedbackRuntime({
                stateModel: oStateModel,
                statePaths: StatePaths,
                feedbackPolicy: FeedbackPolicy,
                bundleText: fnBundleText
            });
            // ZERO-LEGACY: BackendAdapter has been removed. UI5 ODataModel is the single transport.
            DiagnosticsUseCase.execute({}, {
                mainServiceModel: oMainServiceModel,
                stateModel: oStateModel,
                getBackendMode: function () { return "real"; },
                onMetadataFailed: function () {
                    ModelStateRuntime.writeOnModel(oStateModel, "/backendMode", "real");
                }
            });

            this.setModel(oDataModel, "data");
            this.setModel(oMplModel, "mpl");
            this.setModel(oSelectedModel, "selected");
            this.setModel(oStateModel, "state");
            this.setModel(oUiStateModel, "uiState");
            this.setModel(oViewModel, "view");
            this.setModel(oViewModel, "appView");
            this.setModel(oMasterDataModel, "masterData");
            oDeviceModel.setDefaultBindingMode("OneWay");
            this.setModel(oDeviceModel, "device");
            this._oInteractionFX = InteractionFX;
            // Build a Gateway-first context (ports/adapters) for this component.
            this._ctx = CtxFactory.buildCtx(this, {});
            this._detailFacade = new DetailFacade();
            this._actionDispatcher = new ActionDispatcher();
            this._actionDispatcher.setValidators(buildActionValidators(ActionContract));
            var oLayoutModel = reuseJsonModel(this.getModel("layout"), ModelFactory.createLayoutModel);
            var oCacheModel = reuseJsonModel(this.getModel("cache"), ModelFactory.createCacheModel);
            var oEnvModel = ModelFactory.createEnvModel();
            var mTimerDefaults = TimeConfigService.buildDefaultTimerMap();
            var mInitState = { "/timers": mTimerDefaults };
            mInitState[StatePaths.SAVE_IN_FLIGHT] = false;
            mInitState[StatePaths.PENDING_NAVIGATION_INTENT] = null;
            mInitState[StatePaths.TAB_CONFLICT_STATE] = { active: false, source: "", at: "" };
            mInitState["/networkOnline"] = true;
            mInitState["/networkGraceMode"] = false;
            mInitState["/networkGraceExpiresAt"] = null;
            ModelStateRuntime.setManyOnModel(oStateModel, mInitState);
            var fnEmitTelemetry = function (sEventName, oPayload) {
                return WorkflowTelemetry.emit(sEventName, {
                    stateModel: oStateModel,
                    payload: oPayload || {}
                });
            };
            var fnResolveDetailCurrent = function () {
                return ComponentRuntimeSupport.resolveDetailCurrent(oSelectedModel, oUiStateModel);
            };
            var fnApplyFacadeResult = createApplyFacadeResult({
                component: this,
                effectApplier: EffectApplier,
                actionDispatcher: this._actionDispatcher,
                selectedModel: oSelectedModel,
                uiStateModel: oUiStateModel,
                componentRuntimeSupport: ComponentRuntimeSupport
            });
            var fnBuildLatestCtx = function () {
                this._ctx = CtxFactory.buildCtx(this, {});
                return this._ctx;
            }.bind(this);
            var fnHandleForceReadOnly = function (mInput) {
                var mForceInput = Object.assign({}, mInput || {});
                if (!Object.prototype.hasOwnProperty.call(mForceInput, "preserveDirty")) {
                    mForceInput.preserveDirty = !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
                }
                this._oHeartbeat.stop();
                this._oLockStatus.stop();
                this._oAutoSave.stop();
                this._oGcd.destroyManager();
                return this._detailFacade.forceReadOnly(mForceInput, this._ctx).then(function (oResult) {
                    fnApplyFacadeResult(oResult);
                    ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
                    fnEmitTelemetry("lock.lost.detected", TelemetryRuntime.lockLost(
                        mForceInput && mForceInput.reason,
                        mForceInput && mForceInput.source
                    ));
                    return oResult;
                });
            }.bind(this);
            var fnLoadRuntimeSettings = function () {
                return SettingsManager.load(GatewayBackendService).then(function (oRuntime) {
                    return this._applyFrontendRuntimeConfig({
                        source: "RuntimeSettingsSet(GLOBAL)",
                        runtimeSettingsPayload: oRuntime || {}
                    }, oStateModel, oEnvModel, oMasterDataModel).then(function () {
                        ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", "gateway_runtime");
                        fnEmitTelemetry("runtime.config.loaded", TelemetryRuntime.runtimeConfig("RuntimeSettingsSet(GLOBAL)"));
                        return oRuntime || {};
                    });
                }.bind(this)).catch(function (oError) {
                    ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", "gateway_runtime_error");
                    fnEmitTelemetry("runtime.config.load_failed", TelemetryRuntime.runtimeConfig(
                        "RuntimeSettingsSet(GLOBAL)",
                        (oError && oError.message) || oError || "runtime_settings_load_failed"
                    ));
                    // Non-fatal: resolve with empty config so boot sequence continues
                    return {};
                }.bind(this));
            }.bind(this);
            var fnResolveCorrelationId = oFeedbackRuntime.resolveCorrelationId;
            var fnIsSessionExpiredError = oFeedbackRuntime.isSessionExpiredError;
            var fnSetGlobalBanner = oFeedbackRuntime.setGlobalBanner;
            var fnClearGlobalBanner = oFeedbackRuntime.clearGlobalBanner;
            var fnQueuePendingNavigationIntent = function (oRouteEvent) {
                return queuePendingNavigationIntent(this, oStateModel, StatePaths, oRouteEvent);
            }.bind(this);
            var fnClearPendingNavigationIntent = function () {
                return clearPendingNavigationIntent(oStateModel, StatePaths);
            };
            var fnRevertPendingNavigationIntent = function () {
                return ComponentActionRuntime.revertPendingNavigationIntent(this, oStateModel, StatePaths);
            }.bind(this);
            var fnResumePendingNavigationIntent = function () {
                return resumePendingNavigationIntent(this, oStateModel, StatePaths);
            }.bind(this);
            var fnRestorePendingNavigationIntent = function () {
                return ComponentActionRuntime.restorePendingNavigationIntent(this, oStateModel, StatePaths);
            }.bind(this);
            var fnRunGuardedSave = ComponentInitSaveGuardSupport.createRunGuardedSave({
                component: this,
                stateModel: oStateModel,
                mainServiceModel: oMainServiceModel,
                statePaths: StatePaths,
                detailFacade: this._detailFacade,
                buildLatestCtx: fnBuildLatestCtx,
                applyFacadeResult: fnApplyFacadeResult,
                emitTelemetry: fnEmitTelemetry,
                resumePendingNavigationIntent: fnResumePendingNavigationIntent,
                resolveCorrelationId: fnResolveCorrelationId,
                isSessionExpiredError: fnIsSessionExpiredError,
                setGlobalBanner: fnSetGlobalBanner,
                clearGlobalBanner: fnClearGlobalBanner
            });
            var oCrossTabRuntime = attachCrossTabRuntime({
                component: this,
                stateModel: oStateModel,
                statePaths: StatePaths,
                bundleText: fnBundleText,
                setGlobalBanner: fnSetGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly
            });
            var fnPublishTabSignal = oCrossTabRuntime.publishTabSignal;
            registerDefaultHandlers({
                actionDispatcher: this._actionDispatcher,
                actionContract: ActionContract,
                detailFacade: this._detailFacade,
                runGuardedSave: fnRunGuardedSave,
                buildLatestCtx: fnBuildLatestCtx,
                applyFacadeResult: fnApplyFacadeResult,
                getCtx: function () { return this._ctx; }.bind(this)
            });
                        attachManagerRuntime({
                component: this,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                timerDefaults: mTimerDefaults,
                managers: {
                    HeartbeatManager: HeartbeatManager,
                    GCDManager: GCDManager,
                    ActivityMonitor: ActivityMonitor,
                    AutoSaveCoordinator: AutoSaveCoordinator,
                    LockStatusMonitor: LockStatusMonitor
                },
                statePaths: StatePaths,
                deltaPayloadBuilder: DeltaPayloadBuilder,
                resolveDetailCurrent: fnResolveDetailCurrent,
                applyFacadeResult: fnApplyFacadeResult,
                setGlobalBanner: fnSetGlobalBanner,
                emitTelemetry: fnEmitTelemetry,
                debugLogger: DebugLogger,
                actionContract: ActionContract,
                bundleText: fnBundleText,
                componentRuntimeSupport: ComponentRuntimeSupport,
                telemetryRuntime: TelemetryRuntime
            });
            attachLockRuntime({
                component: this,
                mainServiceModel: oMainServiceModel,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                cacheModel: oCacheModel,
                statePaths: StatePaths,
                componentRuntimeSupport: ComponentRuntimeSupport,
                timeConfigService: TimeConfigService,
                debugLogger: DebugLogger,
                bundleText: fnBundleText,
                emitTelemetry: fnEmitTelemetry,
                setGlobalBanner: fnSetGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly,
                applyFacadeResult: fnApplyFacadeResult,
                telemetryRuntime: TelemetryRuntime
            });
            attachInitListeners({
                component: this,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                selectedModel: oSelectedModel,
                layoutModel: oLayoutModel,
                cacheModel: oCacheModel,
                masterDataModel: oMasterDataModel,
                envModel: oEnvModel,
                statePaths: StatePaths,
                smartSearchAdapter: SmartSearchAdapter,
                componentRuntimeSupport: ComponentRuntimeSupport,
                timeConfigService: TimeConfigService,
                flowCoordinator: FlowCoordinator,
                bundleText: fnBundleText,
                setGlobalBanner: fnSetGlobalBanner,
                clearGlobalBanner: fnClearGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly,
                runGuardedSave: fnRunGuardedSave,
                queuePendingNavigationIntent: fnQueuePendingNavigationIntent,
                clearPendingNavigationIntent: fnClearPendingNavigationIntent,
                revertPendingNavigationIntent: fnRevertPendingNavigationIntent,
                resumePendingNavigationIntent: fnResumePendingNavigationIntent,
                restorePendingNavigationIntent: fnRestorePendingNavigationIntent,
                emitTelemetry: fnEmitTelemetry,
                publishTabSignal: fnPublishTabSignal,
                telemetryRuntime: TelemetryRuntime,
                layoutStateRuntime: LayoutStateRuntime,
                actionContract: ActionContract
            });

            runBootSequence({
                component: this,
                stateModel: oStateModel,
                envModel: oEnvModel,
                cacheModel: oCacheModel,
                bootstrapAppUseCase: BootstrapAppUseCase,
                ensureDictLoadedUseCase: EnsureDictLoadedUseCase,
                componentRuntimeSupport: ComponentRuntimeSupport,
                loadRuntimeSettings: fnLoadRuntimeSettings,
                loadCurrentUser: function () {
                    return mDeps.LoadCurrentUserUseCase && mDeps.LoadCurrentUserUseCase.refresh
                        ? mDeps.LoadCurrentUserUseCase.refresh({ stateModel: oStateModel })
                        : Promise.resolve(null);
                },
                bundleText: fnBundleText
            });

    }

    return {
        runInit: runInit
    };
});

