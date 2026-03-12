sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFeedbackBootstrapRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitCompositionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitAttachmentStageRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitStageRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeOptionBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeHandlerBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCrossTabRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentManagerOrchestrationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitListenersRuntime"
], function (ModelStateRuntime, ComponentActionRuntime, ComponentBootRuntime, ComponentFeedbackBootstrapRuntime, ComponentInitCompositionRuntime, ComponentInitAttachmentStageRuntime, ComponentInitStageRuntime, ComponentRuntimeOptionBuilder, ComponentRuntimeHandlerBootstrap, ComponentCrossTabRuntime, ComponentLockEventsRuntime, ComponentManagerOrchestrationRuntime, ComponentInitListenersRuntime) {
    "use strict";

    function buildActionValidators(ActionContract) {
        return ComponentActionRuntime.buildActionValidators(ActionContract);
    }

    function registerDefaultHandlers(mOptions) {
        return ComponentActionRuntime.registerDefaultHandlers(mOptions);
    }

    function createBundleText(component) {
        return ComponentActionRuntime.createBundleText(component);
    }

    function createApplyFacadeResult(mOptions) {
        return ComponentActionRuntime.createApplyFacadeResult(mOptions);
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
        return ComponentInitListenersRuntime.attachInitListeners(mOptions);
    }

    function attachLockRuntime(mOptions) {
        return ComponentLockEventsRuntime.attachLockRuntime(mOptions);
    }

    function attachManagerRuntime(mOptions) {
        return ComponentManagerOrchestrationRuntime.attachManagerRuntime(mOptions);
    }

    function runInit(aInitArgs, mDeps) {
        var UIComponent = mDeps.UIComponent;
        var SmartSearchAdapter = mDeps.SmartSearchAdapter;
        var Managers = mDeps.Managers || {};
        var HeartbeatManager = Managers.HeartbeatManager || mDeps.HeartbeatManager;
        var GCDManager = Managers.GCDManager || mDeps.GCDManager;
        var ActivityMonitor = Managers.ActivityMonitor || mDeps.ActivityMonitor;
        var AutoSaveCoordinator = Managers.AutoSaveCoordinator || mDeps.AutoSaveCoordinator;
        var LockStatusMonitor = Managers.LockStatusMonitor || mDeps.LockStatusMonitor;
        var DeltaPayloadBuilder = mDeps.DeltaPayloadBuilder;
        var DebugLogger = mDeps.DebugLogger;
        var TimeConfigService = mDeps.TimeConfigService;
        var EnsureDictLoadedUseCase = mDeps.EnsureDictLoadedUseCase;
        var BootstrapAppUseCase = mDeps.BootstrapAppUseCase;
        var DiagnosticsUseCase = mDeps.DiagnosticsUseCase;
        var ComponentInitSaveGuardSupport = mDeps.ComponentInitSaveGuardSupport;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var TelemetryRuntime = mDeps.TelemetryRuntime;
        var LayoutStateRuntime = mDeps.LayoutStateRuntime;
        var StatePaths = mDeps.StatePaths;
        var ActionContract = mDeps.ActionContract;
        var ThemeRuntime = mDeps.ThemeRuntime;

            UIComponent.prototype.init.apply(this, aInitArgs || []);
            this._startupPerf = this._startupPerf || {
                t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
                firstRouteReadyLogged: false,
                analyticsStartedLogged: false
            };
            ThemeRuntime.syncDocumentRootClasses();
            var oModelStage = ComponentInitStageRuntime.createModelStage(this, mDeps);
            var mModels = oModelStage.models;
            var oStateModel = mModels.stateModel;
            var oUiStateModel = mModels.uiStateModel;
            var oSelectedModel = mModels.selectedModel;
            var oSnapshotModel = mModels.snapshotModel;
            var oMasterDataModel = mModels.masterDataModel;
            var oLayoutModel = mModels.layoutModel;
            var oCacheModel = mModels.cacheModel;
            var oEnvModel = mModels.envModel;
            var oMainServiceModel = oModelStage.mainServiceModel;
            var fnBundleText = createBundleText(this);
            // ZERO-LEGACY: BackendAdapter has been removed. UI5 ODataModel is the single transport.
            DiagnosticsUseCase.execute({}, {
                mainServiceModel: oMainServiceModel,
                stateModel: oStateModel,
                getBackendMode: function () { return "real"; },
                onMetadataFailed: function () {
                    ModelStateRuntime.writeOnModel(oStateModel, "/backendMode", "real");
                }
            });

            ComponentModelBootstrapRuntime.registerModels(this, mModels);
            var oCoreStage = ComponentInitStageRuntime.createCoreStage(this, mDeps, mModels, {
                buildActionValidators: buildActionValidators,
                createApplyFacadeResult: createApplyFacadeResult
            });
            var oCoreRuntime = oCoreStage.coreRuntime;
            var mTimerDefaults = oCoreStage.timerDefaults;
            var fnEmitTelemetry = oCoreStage.emitTelemetry;
            var oInitContext = ComponentInitCompositionRuntime.createInitContext(this, mDeps, mModels, {
                bundleText: fnBundleText,
                emitTelemetry: fnEmitTelemetry,
                feedbackBootstrap: ComponentFeedbackBootstrapRuntime,
                resumePendingNavigationIntent: resumePendingNavigationIntent
            });
            var oAttachmentContext = ComponentInitAttachmentStageRuntime.createAttachmentContext(this, {
                ActionContract: ActionContract,
                ComponentInitSaveGuardSupport: ComponentInitSaveGuardSupport,
                ComponentRuntimeHandlerBootstrap: ComponentRuntimeHandlerBootstrap,
                ComponentRuntimeSupport: ComponentRuntimeSupport,
                ModelStateRuntime: ModelStateRuntime,
                StatePaths: StatePaths,
                TelemetryRuntime: TelemetryRuntime,
                attachCrossTabRuntime: attachCrossTabRuntime,
                registerDefaultHandlers: registerDefaultHandlers
            }, {
                cacheModel: oCacheModel,
                envModel: oEnvModel,
                layoutModel: oLayoutModel,
                mainServiceModel: oMainServiceModel,
                masterDataModel: oMasterDataModel,
                selectedModel: oSelectedModel,
                snapshotModel: oSnapshotModel,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel
            }, {
                applyFacadeResult: oCoreRuntime.applyFacadeResult,
                buildLatestCtx: oCoreRuntime.buildLatestCtx,
                bundleText: fnBundleText,
                emitTelemetry: fnEmitTelemetry,
                resolveDetailCurrent: oCoreRuntime.resolveDetailCurrent,
                timerDefaults: mTimerDefaults
            }, oInitContext);
            ComponentInitAttachmentStageRuntime.attachRuntimeStages(this, {
                ActionContract: ActionContract,
                ComponentRuntimeOptionBuilder: ComponentRuntimeOptionBuilder,
                ComponentRuntimeSupport: ComponentRuntimeSupport,
                DebugLogger: DebugLogger,
                DeltaPayloadBuilder: DeltaPayloadBuilder,
                FlowCoordinator: FlowCoordinator,
                LayoutStateRuntime: LayoutStateRuntime,
                SmartSearchAdapter: SmartSearchAdapter,
                StatePaths: StatePaths,
                TelemetryRuntime: TelemetryRuntime,
                TimeConfigService: TimeConfigService,
                attachInitListeners: attachInitListeners,
                attachLockRuntime: attachLockRuntime,
                attachManagerRuntime: attachManagerRuntime,
                managers: {
                    HeartbeatManager: HeartbeatManager,
                    GCDManager: GCDManager,
                    ActivityMonitor: ActivityMonitor,
                    AutoSaveCoordinator: AutoSaveCoordinator,
                    LockStatusMonitor: LockStatusMonitor
                }
            }, {
                cacheModel: oCacheModel,
                envModel: oEnvModel,
                layoutModel: oLayoutModel,
                mainServiceModel: oMainServiceModel,
                masterDataModel: oMasterDataModel,
                selectedModel: oSelectedModel,
                snapshotModel: oSnapshotModel,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel
            }, {
                applyFacadeResult: oCoreRuntime.applyFacadeResult,
                buildLatestCtx: oCoreRuntime.buildLatestCtx,
                bundleText: fnBundleText,
                emitTelemetry: fnEmitTelemetry,
                resolveDetailCurrent: oCoreRuntime.resolveDetailCurrent,
                timerDefaults: mTimerDefaults
            }, oAttachmentContext);

            return runBootSequence({
                component: this,
                stateModel: oStateModel,
                envModel: oEnvModel,
                cacheModel: oCacheModel,
                cacheAdapter: this._ctx && this._ctx.cache,
                bootstrapAppUseCase: BootstrapAppUseCase,
                ensureDictLoadedUseCase: EnsureDictLoadedUseCase,
                componentRuntimeSupport: ComponentRuntimeSupport,
                loadRuntimeSettings: oAttachmentContext.runtimeSettingsRuntime.loadRuntimeSettings,
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
