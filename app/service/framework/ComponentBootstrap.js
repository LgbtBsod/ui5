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
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapDependencyBuilder",
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
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentMainServiceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentModelBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCoreRuntimeBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLifecycleBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCoreInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFeedbackRuntime",
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
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapFlowRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentRuntimeOptionsFactory",
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
    ComponentBootstrapDependencyBuilder,
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
    ComponentMainServiceRuntime,
    ComponentModelBootstrap,
    ComponentCoreRuntimeBootstrap,
    ComponentLifecycleBootstrap,
    ComponentBootRuntime,
    ComponentCoreInitRuntime,
    ComponentFeedbackRuntime,
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
    ComponentBootstrapFlowRuntime,
    ComponentRuntimeOptionsFactory,
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

    function createBootstrapDeps() {
        return ComponentBootstrapFlowRuntime.createBootstrapContext({
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
            ComponentFeedbackRuntime: ComponentFeedbackRuntime,
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
        }, ComponentBootstrapDependencyBuilder, ComponentAppRuntime);
    }

    function init(oComponent, aInitArgs) {
        var oBootstrap;
        var mBootstrapDeps;
        var oModelBootstrap;
        var mRuntimeModels;
        var mActionRuntimeOptions;
        var oRuntimeContext;

        ComponentBootstrapFlowRuntime.initializeStartupState(oComponent, UIComponent, InteractionFX, ThemeService, aInitArgs);

        oBootstrap = createBootstrapDeps();
        mBootstrapDeps = oBootstrap.deps;

        oModelBootstrap = ComponentModelBootstrap.bootstrap(oComponent, mBootstrapDeps);
        ComponentBootstrapFlowRuntime.runDiagnostics(DiagnosticsUseCase, BackendModeContracts, mBootstrapDeps, oModelBootstrap);
        ComponentModelInitRuntime.registerModels(oComponent, oModelBootstrap.models);
        oComponent.setModel(oModelBootstrap.mainServiceModel);
        mRuntimeModels = ComponentRuntimeOptionsFactory.buildRuntimeModels(oModelBootstrap);
        mActionRuntimeOptions = ComponentRuntimeOptionsFactory.buildActionRuntimeOptions(oComponent, {
            WorkflowTelemetry: WorkflowTelemetry
        }, oModelBootstrap.models);
        oRuntimeContext = ComponentCoreRuntimeBootstrap.bootstrap(oComponent, Object.assign({}, mBootstrapDeps, mActionRuntimeOptions), mRuntimeModels);

        return ComponentLifecycleBootstrap.bootstrap(
            oComponent,
            ComponentRuntimeOptionsFactory.buildLifecycleDeps(mBootstrapDeps, ComponentBootstrapDependencyBuilder),
            ComponentRuntimeOptionsFactory.buildLifecycleContext(oRuntimeContext, mRuntimeModels)
        );
    }

    return {
        init: init
    };
});
