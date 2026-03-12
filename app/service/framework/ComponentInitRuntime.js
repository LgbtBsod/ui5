sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentModelBootstrapRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentMainServiceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCoreBootstrapRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentStateSeedRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFeedbackBootstrapRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitCompositionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentRuntimeAttachmentBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCrossTabRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLockRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLifecycleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (ModelStateRuntime, FeedbackBannerRuntime, ComponentActionRuntime, ComponentBootRuntime, ComponentModelBootstrapRuntime, ComponentMainServiceRuntime, ComponentCoreBootstrapRuntime, ComponentStateSeedRuntime, ComponentFeedbackBootstrapRuntime, ComponentInitCompositionRuntime, ComponentRuntimeAttachmentBootstrap, ComponentCrossTabRuntime, ComponentLockRuntime, ComponentLifecycleRuntime, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;

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
        return ComponentLifecycleRuntime.attachInitListeners(mOptions);
    }

    function attachLockRuntime(mOptions) {
        return ComponentLockRuntime.attachLockRuntime(mOptions);
    }

    function attachManagerRuntime(mOptions) {
        return ComponentLockRuntime.attachManagerRuntime(mOptions);
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
        var EffectApplier = mDeps.EffectApplier;
        var FeedbackPolicy = mDeps.FeedbackPolicy;
        var ComponentInitSaveGuardSupport = mDeps.ComponentInitSaveGuardSupport;
        var ComponentRuntimeSupport = mDeps.ComponentRuntimeSupport;
        var TelemetryRuntime = mDeps.TelemetryRuntime;
        var LayoutStateRuntime = mDeps.LayoutStateRuntime;
        var StatePaths = mDeps.StatePaths;
        var ActionContract = mDeps.ActionContract;
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
            var sMainServiceUri = this.getManifestEntry("/sap.app/dataSources/mainService/uri") || "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/";
            var mModels = ComponentModelBootstrapRuntime.createModels(this, mDeps);
            var oStateModel = mModels.stateModel;
            var oUiStateModel = mModels.uiStateModel;
            var oSelectedModel = mModels.selectedModel;
            var oSnapshotModel = mModels.snapshotModel;
            var oMasterDataModel = mModels.masterDataModel;
            var oLayoutModel = mModels.layoutModel;
            var oCacheModel = mModels.cacheModel;
            var oEnvModel = mModels.envModel;
            var oMainServiceModel = ComponentMainServiceRuntime.createMainServiceModel(this, mDeps, sMainServiceUri);
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
            var oCoreRuntime = ComponentCoreBootstrapRuntime.bootstrapComponentRuntime(this, mDeps, mModels, {
                buildActionValidators: buildActionValidators,
                createApplyFacadeResult: createApplyFacadeResult
            });
            var mTimerDefaults = ComponentStateSeedRuntime.seedInitialState(oStateModel, StatePaths, TimeConfigService);
            var fnEmitTelemetry = function (sEventName, oPayload) {
                return WorkflowTelemetry.emit(sEventName, {
                    stateModel: oStateModel,
                    payload: oPayload || {}
                });
            };
            var oInitContext = ComponentInitCompositionRuntime.createInitContext(this, mDeps, mModels, {
                bundleText: fnBundleText,
                emitTelemetry: fnEmitTelemetry,
                feedbackBootstrap: ComponentFeedbackBootstrapRuntime,
                resumePendingNavigationIntent: resumePendingNavigationIntent
            });
            var fnResolveDetailCurrent = oCoreRuntime.resolveDetailCurrent;
            var fnApplyFacadeResult = oCoreRuntime.applyFacadeResult;
            var fnBuildLatestCtx = oCoreRuntime.buildLatestCtx;
            var fnHandleForceReadOnly = ComponentRuntimeAttachmentBootstrap.createForceReadOnlyHandler({
                component: this,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                statePaths: StatePaths,
                componentRuntimeSupport: ComponentRuntimeSupport,
                telemetryRuntime: TelemetryRuntime,
                applyFacadeResult: fnApplyFacadeResult,
                emitTelemetry: fnEmitTelemetry,
                readDirty: function () {
                    return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
                }
            });
            var oRuntimeSettingsRuntime = oInitContext.runtimeSettingsRuntime;
            var fnResolveCorrelationId = oInitContext.feedbackRuntime.resolveCorrelationId;
            var fnIsSessionExpiredError = oInitContext.feedbackRuntime.isSessionExpiredError;
            var fnSetGlobalBanner = oInitContext.feedbackRuntime.setGlobalBanner;
            var fnClearGlobalBanner = oInitContext.feedbackRuntime.clearGlobalBanner;
            var oPendingNavigationRuntime = oInitContext.pendingNavigationRuntime;
            var fnQueuePendingNavigationIntent = oPendingNavigationRuntime.queuePendingNavigationIntent;
            var fnClearPendingNavigationIntent = oPendingNavigationRuntime.clearPendingNavigationIntent;
            var fnRevertPendingNavigationIntent = oPendingNavigationRuntime.revertPendingNavigationIntent;
            var fnResumePendingNavigationIntent = oPendingNavigationRuntime.resumePendingNavigationIntent;
            var fnRestorePendingNavigationIntent = oPendingNavigationRuntime.restorePendingNavigationIntent;
            var fnRunGuardedSave = ComponentRuntimeAttachmentBootstrap.createGuardedSave({
                component: this,
                stateModel: oStateModel,
                mainServiceModel: oMainServiceModel,
                statePaths: StatePaths,
                buildLatestCtx: fnBuildLatestCtx,
                applyFacadeResult: fnApplyFacadeResult,
                emitTelemetry: fnEmitTelemetry,
                resumePendingNavigationIntent: fnResumePendingNavigationIntent,
                resolveCorrelationId: fnResolveCorrelationId,
                isSessionExpiredError: fnIsSessionExpiredError,
                setGlobalBanner: fnSetGlobalBanner,
                clearGlobalBanner: fnClearGlobalBanner,
                saveGuardSupport: ComponentInitSaveGuardSupport
            });
            var oCrossTabRuntime = ComponentRuntimeAttachmentBootstrap.registerCrossTabRuntime({
                component: this,
                stateModel: oStateModel,
                statePaths: StatePaths,
                bundleText: fnBundleText,
                setGlobalBanner: fnSetGlobalBanner,
                handleForceReadOnly: fnHandleForceReadOnly,
                attachCrossTabRuntime: attachCrossTabRuntime
            }).crossTabRuntime;
            var fnPublishTabSignal = oCrossTabRuntime.publishTabSignal;
            ComponentRuntimeAttachmentBootstrap.registerDefaultHandlers({
                component: this,
                actionContract: ActionContract,
                runGuardedSave: fnRunGuardedSave,
                buildLatestCtx: fnBuildLatestCtx,
                applyFacadeResult: fnApplyFacadeResult,
                registerDefaultHandlers: registerDefaultHandlers
            });
            attachManagerRuntime(ComponentRuntimeAttachmentBootstrap.buildManagerRuntimeOptions({
                component: this,
                stateModel: oStateModel,
                uiStateModel: oUiStateModel,
                snapshotModel: oSnapshotModel,
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
            }));
            attachLockRuntime(ComponentRuntimeAttachmentBootstrap.buildLockRuntimeOptions({
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
            }));
            attachInitListeners(ComponentRuntimeAttachmentBootstrap.buildListenerRuntimeOptions({
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
            }));

            return runBootSequence({
                component: this,
                stateModel: oStateModel,
                envModel: oEnvModel,
                cacheModel: oCacheModel,
                cacheAdapter: this._ctx && this._ctx.cache,
                bootstrapAppUseCase: BootstrapAppUseCase,
                ensureDictLoadedUseCase: EnsureDictLoadedUseCase,
                componentRuntimeSupport: ComponentRuntimeSupport,
                loadRuntimeSettings: oRuntimeSettingsRuntime.loadRuntimeSettings,
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
