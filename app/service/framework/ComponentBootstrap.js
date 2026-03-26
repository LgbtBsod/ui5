sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/model/json/JSONModel",
    "sap/ui/Device",
    "PRODUCTION_CONTROL_CHECKLIST/model/ModelFactory",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/PollingManager",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/GCDManager",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/ActivityMonitor",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/AutoSaveCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/ConnectivityCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/SettingsManager",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeTimerSanitizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectApplier",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentAutosaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockReleaseRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSaveGuardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentModelInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLifecycleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentRuntimeSettingsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentPollingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCrossTabRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentInitListenersRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionDispatcher",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/InteractionFX",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAppRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFacadeEffectRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/SearchUiConfig",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/ApplyRuntimeSettingsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/EnsureDictLoadedUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/InitializeAppUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/DiagnosticsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/BackendModeContracts"
], function (
    UIComponent,
    JSONModel,
    Device,
    ModelFactory,
    StatePaths,
    ModelStateRuntime,
    PollingManager,
    GCDManager,
    ActivityMonitor,
    AutoSaveCoordinator,
    ConnectivityCoordinator,
    SettingsManager,
    DeltaPayloadBuilder,
    CreateSentinel,
    GatewayClient,
    DebugLogger,
    RuntimeTimerSanitizer,
    TimeConfigService,
    EffectApplier,
    FeedbackPolicy,
    WorkflowCoordinator,
    ComponentAutosaveRuntime,
    ComponentLockReleaseRuntime,
    ComponentSaveGuardRuntime,
    ComponentModelInitRuntime,
    ComponentLifecycleRuntime,
    ComponentRuntimeSettingsRuntime,
    ComponentPollingRuntime,
    ComponentCrossTabRuntime,
    ComponentInitListenersRuntime,
    ComponentLockEventsRuntime,
    TelemetryRuntime,
    LayoutStateRuntime,
    ActionDispatcher,
    ActionContract,
    WorkflowTelemetry,
    InteractionFX,
    ThemeService,
    ComponentAppRuntime,
    ComponentFacadeEffectRuntime,
    DetailFacade,
    SearchUiConfig,
    ApplyRuntimeSettingsUseCase,
    EnsureDictLoadedUseCase,
    InitializeAppUseCase,
    LoadCurrentUserUseCase,
    DiagnosticsUseCase,
    BackendModeContracts
) {
    "use strict";

    function buildGroupedDependencies(mStaticDeps) {
        return {
            core: {
                UIComponent: mStaticDeps.UIComponent,
                JSONModel: mStaticDeps.JSONModel,
                Device: mStaticDeps.Device,
                ModelFactory: mStaticDeps.ModelFactory,
                ModelStateRuntime: mStaticDeps.ModelStateRuntime,
                GatewayClient: mStaticDeps.GatewayClient,
                DebugLogger: mStaticDeps.DebugLogger,
                RuntimeTimerSanitizer: mStaticDeps.RuntimeTimerSanitizer,
                TimeConfigService: mStaticDeps.TimeConfigService,
                EffectApplier: mStaticDeps.EffectApplier,
                FeedbackPolicy: mStaticDeps.FeedbackPolicy,
                WorkflowCoordinator: mStaticDeps.WorkflowCoordinator,
                TelemetryRuntime: mStaticDeps.TelemetryRuntime,
                LayoutStateRuntime: mStaticDeps.LayoutStateRuntime,
                ActionDispatcher: mStaticDeps.ActionDispatcher,
                ActionContract: mStaticDeps.ActionContract,
                WorkflowTelemetry: mStaticDeps.WorkflowTelemetry,
                CreateSentinel: mStaticDeps.CreateSentinel,
                DeltaPayloadBuilder: mStaticDeps.DeltaPayloadBuilder,
                StatePaths: mStaticDeps.StatePaths,
                SearchUiConfig: mStaticDeps.SearchUiConfig,
                DetailFacade: mStaticDeps.DetailFacade
            },
            managers: {
                Managers: Object.freeze({
                    PollingManager: mStaticDeps.PollingManager,
                    GCDManager: mStaticDeps.GCDManager,
                    ActivityMonitor: mStaticDeps.ActivityMonitor,
                    AutoSaveCoordinator: mStaticDeps.AutoSaveCoordinator,
                    ConnectivityCoordinator: mStaticDeps.ConnectivityCoordinator,
                    SettingsManager: mStaticDeps.SettingsManager
                }),
                managers: {}
            },
            runtime: {
                ComponentAutosaveRuntime: ComponentAutosaveRuntime,
                ComponentLockReleaseRuntime: ComponentLockReleaseRuntime,
                ComponentSaveGuardRuntime: ComponentSaveGuardRuntime,
                ComponentModelInitRuntime: ComponentModelInitRuntime,
                ComponentRuntimeSettingsRuntime: ComponentRuntimeSettingsRuntime,
                ComponentPollingRuntime: ComponentPollingRuntime,
                ComponentCrossTabRuntime: ComponentCrossTabRuntime,
                ComponentInitListenersRuntime: ComponentInitListenersRuntime,
                ComponentLockEventsRuntime: ComponentLockEventsRuntime,
                ComponentLifecycleRuntime: ComponentLifecycleRuntime
            },
            theme: {
                ThemeRuntime: ThemeService
            },
            usecases: {
                ApplyRuntimeSettingsUseCase: ApplyRuntimeSettingsUseCase,
                EnsureDictLoadedUseCase: EnsureDictLoadedUseCase,
                InitializeAppUseCase: InitializeAppUseCase,
                LoadCurrentUserUseCase: LoadCurrentUserUseCase,
                DiagnosticsUseCase: DiagnosticsUseCase
            }
        };
    }

    function flattenDependencyGroups(mGroups) {
        return Object.assign({},
            mGroups.core || {},
            mGroups.managers || {},
            mGroups.runtime || {},
            mGroups.theme || {},
            mGroups.usecases || {}
        );
    }

    function attachManagerRuntime(mDeps) {
        var mResolved = Object.assign({}, mDeps);
        var oManagers = mResolved.Managers || {};
        mResolved.managers = {};
        mResolved.managers.GCDManager = oManagers.GCDManager;
        mResolved.managers.ActivityMonitor = oManagers.ActivityMonitor;
        mResolved.managers.AutoSaveCoordinator = oManagers.AutoSaveCoordinator;
        return mResolved;
    }

    function buildActionRuntimeOptions(oComponent, mDeps, mModels) {
        return {
            bundleText: ComponentFacadeEffectRuntime.createBundleText(oComponent),
            emitTelemetry: function (sEventName, oPayload) {
                return mDeps.WorkflowTelemetry.emit(sEventName, {
                    stateModel: mModels.stateModel,
                    payload: oPayload || {}
                });
            }
        };
    }

    function buildRuntimeModels(mModelBootstrap) {
        return Object.assign({}, mModelBootstrap.models, {
            mainServiceModel: mModelBootstrap.mainServiceModel
        });
    }

    function createMainServiceModel(oComponent, mDeps) {
        var oMainServiceModel = oComponent && oComponent.getModel ? oComponent.getModel("mainService") : null;
        var sManifestUri = oComponent && oComponent.getManifestEntry
            ? oComponent.getManifestEntry("/sap.app/dataSources/mainService/uri")
            : "";
        var sResolvedServiceUrl;

        if (!sManifestUri) {
            throw new Error("Manifest-driven mainService dataSource is missing. Check sap.app/dataSources/mainService/uri in manifest.json.");
        }
        if (!oMainServiceModel) {
            throw new Error("Manifest-owned mainService model is missing on the component");
        }

        sResolvedServiceUrl = String((oMainServiceModel && oMainServiceModel.sServiceUrl) || sManifestUri || "").replace(/\/+$/, "");
        mDeps.GatewayClient.setModel(oMainServiceModel, { serviceUrl: sResolvedServiceUrl || sManifestUri });

        return oMainServiceModel;
    }

    function bootstrapModels(oComponent, mDeps) {
        return {
            models: mDeps.ComponentModelInitRuntime.initializeModels(oComponent, mDeps),
            mainServiceModel: createMainServiceModel(oComponent, mDeps)
        };
    }

    function createBootstrapDeps() {
        var mStaticDeps = {
            UIComponent: UIComponent,
            JSONModel: JSONModel,
            Device: Device,
            ModelFactory: ModelFactory,
            StatePaths: StatePaths,
            ModelStateRuntime: ModelStateRuntime,
            PollingManager: PollingManager,
            GCDManager: GCDManager,
            ActivityMonitor: ActivityMonitor,
            AutoSaveCoordinator: AutoSaveCoordinator,
            ConnectivityCoordinator: ConnectivityCoordinator,
            SettingsManager: SettingsManager,
            DeltaPayloadBuilder: DeltaPayloadBuilder,
            CreateSentinel: CreateSentinel,
            GatewayClient: GatewayClient,
            DebugLogger: DebugLogger,
            RuntimeTimerSanitizer: RuntimeTimerSanitizer,
            TimeConfigService: TimeConfigService,
            EffectApplier: EffectApplier,
            FeedbackPolicy: FeedbackPolicy,
            WorkflowCoordinator: WorkflowCoordinator,
            ComponentAutosaveRuntime: ComponentAutosaveRuntime,
            TelemetryRuntime: TelemetryRuntime,
            LayoutStateRuntime: LayoutStateRuntime,
            ActionDispatcher: ActionDispatcher,
            ActionContract: ActionContract,
            WorkflowTelemetry: WorkflowTelemetry,
            ComponentRuntimeSettingsRuntime: ComponentRuntimeSettingsRuntime,
            ComponentPollingRuntime: ComponentPollingRuntime,
            ThemeService: ThemeService,
            SearchUiConfig: SearchUiConfig,
            DetailFacade: DetailFacade,
            ApplyRuntimeSettingsUseCase: ApplyRuntimeSettingsUseCase,
            EnsureDictLoadedUseCase: EnsureDictLoadedUseCase,
            InitializeAppUseCase: InitializeAppUseCase,
            LoadCurrentUserUseCase: LoadCurrentUserUseCase,
            DiagnosticsUseCase: DiagnosticsUseCase
        };
        var mGroups = buildGroupedDependencies(mStaticDeps);
        var mDeps = flattenDependencyGroups(mGroups);

        mDeps = attachManagerRuntime(mDeps);
        mDeps.ComponentRuntimeSupport = ComponentAppRuntime.buildComponentRuntimeSupport();

        return {
            groups: mGroups,
            deps: mDeps,
            getBackendMode: function () { return BackendModeContracts.MODES.REAL; }
        };
    }

    function initializeStartupState(oComponent, aInitArgs) {
        oComponent._oInteractionFX = InteractionFX;
        UIComponent.prototype.init.apply(oComponent, aInitArgs || []);
        oComponent._startupPerf = oComponent._startupPerf || {
            t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
            firstRouteReadyLogged: false,
            analyticsStartedLogged: false
        };
        ThemeService.syncDocumentRootClasses();
    }

    function runDiagnostics(oDiagnosticsUseCase, mBootstrapDeps, oModelBootstrap) {
        return oDiagnosticsUseCase.execute({}, {
            mainServiceModel: oModelBootstrap.mainServiceModel,
            stateModel: oModelBootstrap.models.stateModel,
            getBackendMode: function () {
                return BackendModeContracts.MODES.REAL;
            },
            onMetadataFailed: function () {
                mBootstrapDeps.ModelStateRuntime.writeOnModel(
                    oModelBootstrap.models.stateModel,
                    BackendModeContracts.PATHS.BACKEND_MODE,
                    BackendModeContracts.MODES.REAL
                );
            }
        });
    }

    function init(oComponent, aInitArgs) {
        var oBootstrap;
        var mBootstrapDeps;
        var oModelBootstrap;
        var mRuntimeModels;
        var mActionRuntimeOptions;
        var oNavigationRuntime;

        initializeStartupState(oComponent, aInitArgs);

        oBootstrap = createBootstrapDeps();
        mBootstrapDeps = oBootstrap.deps;

        oModelBootstrap = bootstrapModels(oComponent, mBootstrapDeps);
        ModelStateRuntime.writeOnModel(oModelBootstrap.models.stateModel, BackendModeContracts.PATHS.BACKEND_MODE, oBootstrap.getBackendMode());
        runDiagnostics(DiagnosticsUseCase, mBootstrapDeps, oModelBootstrap);
        ComponentModelInitRuntime.registerModels(oComponent, oModelBootstrap.models);
        oComponent.setModel(oModelBootstrap.mainServiceModel);
        oComponent._detailFacade = oComponent._detailFacade || new DetailFacade();
        mRuntimeModels = buildRuntimeModels(oModelBootstrap);
        mActionRuntimeOptions = buildActionRuntimeOptions(oComponent, {
            WorkflowTelemetry: WorkflowTelemetry
        }, oModelBootstrap.models);
        oNavigationRuntime = ComponentLifecycleRuntime.attachRuntime(
            oComponent,
            Object.assign({}, mBootstrapDeps, mActionRuntimeOptions),
            mRuntimeModels
        );

        return ComponentLifecycleRuntime.runBootSequence({
            component: oComponent,
            stateModel: oModelBootstrap.models.stateModel,
            shellModel: oModelBootstrap.models.shellModel,
            envState: oModelBootstrap.models.envState,
            cacheState: oModelBootstrap.models.cacheState,
            cacheAdapter: oComponent._ctx && oComponent._ctx.cache,
            initializeAppUseCase: InitializeAppUseCase,
            ensureDictLoadedUseCase: EnsureDictLoadedUseCase,
            componentRuntimeSupport: mBootstrapDeps.ComponentRuntimeSupport,
            loadRuntimeSettings: function () {
                return mBootstrapDeps.ComponentRuntimeSettingsRuntime.loadRuntimeSettings({
                    stateModel: oModelBootstrap.models.stateModel,
                    envState: oModelBootstrap.models.envState,
                    masterDataModel: oModelBootstrap.models.masterDataModel,
                    settingsManager: mBootstrapDeps.Managers && mBootstrapDeps.Managers.SettingsManager || mBootstrapDeps.SettingsManager,
                    gatewayBackendService: GatewayClient,
                    telemetryRuntime: TelemetryRuntime,
                    emitTelemetry: mActionRuntimeOptions.emitTelemetry
                });
            },
            loadCurrentUser: function () {
                return LoadCurrentUserUseCase && LoadCurrentUserUseCase.refresh
                    ? LoadCurrentUserUseCase.refresh({ stateModel: oModelBootstrap.models.stateModel })
                    : Promise.resolve(null);
            },
            bundleText: mActionRuntimeOptions.bundleText
        }).then(function () {
            return oNavigationRuntime;
        });
    }

    return {
        init: init
    };
});
