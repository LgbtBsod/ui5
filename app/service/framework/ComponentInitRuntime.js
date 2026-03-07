sap.ui.define([], function () {
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

    function runInit(aInitArgs, mDeps) {
        var UIComponent = mDeps.UIComponent;
        var ModelFactory = mDeps.ModelFactory;
        var SmartSearchAdapter = mDeps.SmartSearchAdapter;
        var Managers = mDeps.Managers || {};
        var SmartCacheManager = Managers.SmartCacheManager || mDeps.SmartCacheManager;
        var HeartbeatManager = Managers.HeartbeatManager || mDeps.HeartbeatManager;
        var GCDManager = Managers.GCDManager || mDeps.GCDManager;
        var ActivityMonitor = Managers.ActivityMonitor || mDeps.ActivityMonitor;
        var AutoSaveCoordinator = Managers.AutoSaveCoordinator || mDeps.AutoSaveCoordinator;
        var ConnectivityCoordinator = Managers.ConnectivityCoordinator || mDeps.ConnectivityCoordinator;
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
        var ComponentInitFeedbackSupport = mDeps.ComponentInitFeedbackSupport;
        var ComponentInitSaveGuardSupport = mDeps.ComponentInitSaveGuardSupport;
        var ComponentInitLockRuntimeSupport = mDeps.ComponentInitLockRuntimeSupport;
        var ComponentInitListenersSupport = mDeps.ComponentInitListenersSupport;
        var ComponentInitBootSupport = mDeps.ComponentInitBootSupport;
        var ComponentInitActionRoutingSupport = mDeps.ComponentInitActionRoutingSupport;
        var ComponentInitCrossTabSupport = mDeps.ComponentInitCrossTabSupport;
        var ComponentInitManagerRuntimeSupport = mDeps.ComponentInitManagerRuntimeSupport;
        var ComponentInitRuntimeSupport = mDeps.ComponentInitRuntimeSupport;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var TelemetryRuntime = mDeps.TelemetryRuntime;
        var StatePaths = mDeps.StatePaths;
        var DetailFacade = mDeps.DetailFacade;
        var ActionDispatcher = mDeps.ActionDispatcher;
        var ActionContract = mDeps.ActionContract;
        var ODataModel = mDeps.ODataModel;
        var WorkflowTelemetry = mDeps.WorkflowTelemetry;
        var CreateSentinel = mDeps.CreateSentinel;
        var Device = mDeps.Device;
        var InteractionFX = mDeps.InteractionFX;
        var ThemeService = mDeps.ThemeService;

            UIComponent.prototype.init.apply(this, aInitArgs || []);
            this._startupPerf = this._startupPerf || {
                t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
                firstRouteReadyLogged: false,
                analyticsStartedLogged: false
            };
            ThemeService.syncDocumentRootClasses();
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
            var fnBundleText = ComponentInitRuntimeSupport.createBundleText(this);
            var oFeedbackRuntime = ComponentInitFeedbackSupport.create({
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
                    oStateModel.setProperty("/backendMode", "real");
                }
            });

            this.setModel(oDataModel, "data");
            this.setModel(oMplModel, "mpl");
            this.setModel(oSelectedModel, "selected");
            this.setModel(oStateModel, "state");
            this.setModel(oUiStateModel, "uiState");
            this.setModel(oViewModel, "view");
            this.setModel(oMasterDataModel, "masterData");
            oDeviceModel.setDefaultBindingMode("OneWay");
            this.setModel(oDeviceModel, "device");
            this._oInteractionFX = InteractionFX;
            // Build a Gateway-first context (ports/adapters) for this component.
            this._ctx = CtxFactory.buildCtx(this, {});
            this._detailFacade = new DetailFacade();
            this._actionDispatcher = new ActionDispatcher();
            this._actionDispatcher.setValidators(ComponentInitActionRoutingSupport.buildActionValidators(ActionContract));
            var oLayoutModel = reuseJsonModel(this.getModel("layout"), ModelFactory.createLayoutModel);
            var oCacheModel = reuseJsonModel(this.getModel("cache"), ModelFactory.createCacheModel);
            var oEnvModel = ModelFactory.createEnvModel();
            var mTimerDefaults = TimeConfigService.buildDefaultTimerMap();
            oStateModel.setProperty("/timers", mTimerDefaults);
            oStateModel.setProperty(StatePaths.SAVE_IN_FLIGHT, false);
            oStateModel.setProperty(StatePaths.PENDING_NAVIGATION_INTENT, null);
            oStateModel.setProperty(StatePaths.TAB_CONFLICT_STATE, { active: false, source: "", at: "" });
            var fnEmitTelemetry = function (sEventName, oPayload) {
                return WorkflowTelemetry.emit(sEventName, {
                    stateModel: oStateModel,
                    payload: oPayload || {}
                });
            };
            var fnResolveDetailCurrent = function () {
                return ComponentRuntimeSupport.resolveDetailCurrent(oSelectedModel, oUiStateModel);
            };
            var fnApplyFacadeResult = ComponentInitRuntimeSupport.createApplyFacadeResult({
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
                    mForceInput.preserveDirty = !!oStateModel.getProperty(StatePaths.WORKFLOW_DIRTY);
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
                        oStateModel.setProperty("/frontendConfigSource", "gateway_runtime");
                        fnEmitTelemetry("runtime.config.loaded", TelemetryRuntime.runtimeConfig("RuntimeSettingsSet(GLOBAL)"));
                        return oRuntime || {};
                    });
                }.bind(this)).catch(function (oError) {
                    oStateModel.setProperty("/frontendConfigSource", "gateway_runtime_error");
                    fnEmitTelemetry("runtime.config.load_failed", TelemetryRuntime.runtimeConfig(
                        "RuntimeSettingsSet(GLOBAL)",
                        (oError && oError.message) || oError || "runtime_settings_load_failed"
                    ));
                    return Promise.reject(oError);
                }.bind(this));
            }.bind(this);
            var fnResolveCorrelationId = oFeedbackRuntime.resolveCorrelationId;
            var fnIsSessionExpiredError = oFeedbackRuntime.isSessionExpiredError;
            var fnSetGlobalBanner = oFeedbackRuntime.setGlobalBanner;
            var fnClearGlobalBanner = oFeedbackRuntime.clearGlobalBanner;
            var fnQueuePendingNavigationIntent = function (oRouteEvent) {
                return ComponentInitRuntimeSupport.queuePendingNavigationIntent(oStateModel, StatePaths, oRouteEvent);
            };
            var fnClearPendingNavigationIntent = function () {
                return ComponentInitRuntimeSupport.clearPendingNavigationIntent(oStateModel, StatePaths);
            };
            var fnResumePendingNavigationIntent = function () {
                return ComponentInitRuntimeSupport.resumePendingNavigationIntent(this, oStateModel, StatePaths);
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
            var oCrossTabRuntime = ComponentInitCrossTabSupport.attach({
                component: this,
                stateModel: oStateModel,
                statePaths: StatePaths,
                bundleText: fnBundleText,
                setGlobalBanner: fnSetGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly
            });
            var fnPublishTabSignal = oCrossTabRuntime.publishTabSignal;
            ComponentInitActionRoutingSupport.registerDefaultHandlers({
                actionDispatcher: this._actionDispatcher,
                actionContract: ActionContract,
                detailFacade: this._detailFacade,
                runGuardedSave: fnRunGuardedSave,
                buildLatestCtx: fnBuildLatestCtx,
                applyFacadeResult: fnApplyFacadeResult,
                getCtx: function () { return this._ctx; }.bind(this)
            });
                        ComponentInitManagerRuntimeSupport.attach({
                component: this,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                timerDefaults: mTimerDefaults,
                managers: {
                    SmartCacheManager: SmartCacheManager,
                    HeartbeatManager: HeartbeatManager,
                    GCDManager: GCDManager,
                    ActivityMonitor: ActivityMonitor,
                    AutoSaveCoordinator: AutoSaveCoordinator,
                    ConnectivityCoordinator: ConnectivityCoordinator,
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
                componentRuntimeSupport: ComponentRuntimeSupport
            });
            ComponentInitLockRuntimeSupport.attach({
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
                applyFacadeResult: fnApplyFacadeResult
            });
            ComponentInitListenersSupport.attach({
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
                resumePendingNavigationIntent: fnResumePendingNavigationIntent,
                emitTelemetry: fnEmitTelemetry,
                publishTabSignal: fnPublishTabSignal
            });

            ComponentInitBootSupport.run({
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

