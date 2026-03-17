sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFeedbackInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitCompositionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAttachmentContextRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeAttachOrchestrator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitStageRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeOptionBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeHandlerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCrossTabRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentManagerOrchestrationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitListenersRuntime"
], function (ModelStateRuntime, ComponentActionRuntime, ComponentBootRuntime, ComponentFeedbackInitRuntime, ComponentInitCompositionRuntime, ComponentAttachmentContextRuntime, ComponentRuntimeAttachOrchestrator, ComponentInitStageRuntime, ComponentRuntimeOptionBuilder, ComponentRuntimeHandlerRuntime, ComponentCrossTabRuntime, ComponentLockEventsRuntime, ComponentManagerOrchestrationRuntime, ComponentInitListenersRuntime) {
    "use strict";

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
        var InitializeAppUseCase = mDeps.InitializeAppUseCase;
        var DiagnosticsUseCase = mDeps.DiagnosticsUseCase;
        var ComponentSaveGuardRuntime = mDeps.ComponentSaveGuardRuntime;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var ComponentModelInitRuntime = mDeps.ComponentModelInitRuntime;
        var TelemetryRuntime = mDeps.TelemetryRuntime;
        var LayoutStateRuntime = mDeps.LayoutStateRuntime;
        var StatePaths = mDeps.StatePaths;
        var ActionContract = mDeps.ActionContract;
        var ThemeRuntime = mDeps.ThemeRuntime;
        var WorkflowCoordinator = mDeps.WorkflowCoordinator;

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
            var fnBundleText = ComponentActionRuntime.createBundleText(this);
            // ZERO-LEGACY: BackendAdapter has been removed. UI5 ODataModel is the single transport.
            DiagnosticsUseCase.execute({}, {
                mainServiceModel: oMainServiceModel,
                stateModel: oStateModel,
                getBackendMode: function () { return "real"; },
                onMetadataFailed: function () {
                    ModelStateRuntime.writeOnModel(oStateModel, "/backendMode", "real");
                }
            });

            ComponentModelInitRuntime.registerModels(this, mModels);
            var oCoreStage = ComponentInitStageRuntime.createCoreStage(this, mDeps, mModels, {
                buildActionValidators: ComponentActionRuntime.buildActionValidators,
                createApplyFacadeResult: ComponentActionRuntime.createApplyFacadeResult
            });
            var oCoreRuntime = oCoreStage.coreRuntime;
            var mTimerDefaults = oCoreStage.timerDefaults;
            var fnEmitTelemetry = oCoreStage.emitTelemetry;
            var oInitContext = ComponentInitCompositionRuntime.createInitContext(this, mDeps, mModels, {
                bundleText: fnBundleText,
                emitTelemetry: fnEmitTelemetry,
            feedbackBootstrap: ComponentFeedbackInitRuntime,
                resumePendingNavigationIntent: ComponentActionRuntime.resumePendingNavigationIntent
            });
            var oAttachmentContext = ComponentAttachmentContextRuntime.createAttachmentContext(this, {
                ActionContract: ActionContract,
                saveGuardRuntime: ComponentSaveGuardRuntime,
            ComponentRuntimeHandlerRuntime: ComponentRuntimeHandlerRuntime,
                ComponentRuntimeSupport: ComponentRuntimeSupport,
                ModelStateRuntime: ModelStateRuntime,
                StatePaths: StatePaths,
                TelemetryRuntime: TelemetryRuntime,
                attachCrossTabRuntime: ComponentCrossTabRuntime.attachCrossTabRuntime,
                registerDefaultHandlers: ComponentActionRuntime.registerDefaultHandlers
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
            ComponentRuntimeAttachOrchestrator.attachRuntimeStages(this, {
                ActionContract: ActionContract,
                ComponentRuntimeOptionBuilder: ComponentRuntimeOptionBuilder,
                ComponentRuntimeSupport: ComponentRuntimeSupport,
                DebugLogger: DebugLogger,
                DeltaPayloadBuilder: DeltaPayloadBuilder,
                WorkflowCoordinator: WorkflowCoordinator,
                LayoutStateRuntime: LayoutStateRuntime,
                SmartSearchAdapter: SmartSearchAdapter,
                StatePaths: StatePaths,
                TelemetryRuntime: TelemetryRuntime,
                TimeConfigService: TimeConfigService,
                attachInitListeners: ComponentInitListenersRuntime.attachInitListeners,
                attachLockRuntime: ComponentLockEventsRuntime.attachLockRuntime,
                attachManagerRuntime: ComponentManagerOrchestrationRuntime.attachManagerRuntime,
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

            return ComponentBootRuntime.runBootSequence({
                component: this,
                stateModel: oStateModel,
                envModel: oEnvModel,
                cacheModel: oCacheModel,
                cacheAdapter: this._ctx && this._ctx.cache,
                initializeAppUseCase: InitializeAppUseCase,
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
