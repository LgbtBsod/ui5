sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/model/json/JSONModel",
    "sap/ui/Device",
    "PRODUCTION_CONTROL_CHECKLIST/model/ModelFactory",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/ManagerFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapDependencyBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeTimerSanitizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectApplier",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockReleaseRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSaveGuardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentModelInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentMainServiceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentStateSeedRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentModelBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCoreRuntimeBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLifecycleBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCoreInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentActionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFeedbackInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentRuntimeHandlerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCrossTabRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentInitListenersRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentManagerOrchestrationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionDispatcher",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/InteractionFX",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAppRuntime",
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
    ManagerFacade,
    ComponentBootstrapDependencyBuilder,
    ComponentBootstrapContracts,
    DeltaPayloadBuilder,
    CreateSentinel,
    GatewayClient,
    DebugLogger,
    RuntimeTimerSanitizer,
    TimeConfigService,
    CtxFactory,
    EffectApplier,
    FeedbackPolicy,
    WorkflowCoordinator,
    ComponentLockReleaseRuntime,
    ComponentSaveGuardRuntime,
    ComponentModelInitRuntime,
    ComponentMainServiceRuntime,
    ComponentStateSeedRuntime,
    ComponentModelBootstrap,
    ComponentCoreRuntimeBootstrap,
    ComponentLifecycleBootstrap,
    ComponentBootRuntime,
    ComponentCoreInitRuntime,
    ComponentActionRuntime,
    ComponentFeedbackInitRuntime,
    ComponentRuntimeHandlerRuntime,
    ComponentCrossTabRuntime,
    ComponentInitListenersRuntime,
    ComponentManagerOrchestrationRuntime,
    ComponentLockEventsRuntime,
    TelemetryRuntime,
    LayoutStateRuntime,
    ActionDispatcher,
    ActionContract,
    WorkflowTelemetry,
    InteractionFX,
    ThemeService,
    ComponentAppRuntime,
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

    function createBootstrapDeps(oComponent) {
        var mGroups = ComponentBootstrapDependencyBuilder.build({
            UIComponent: UIComponent,
            JSONModel: JSONModel,
            Device: Device,
            ModelFactory: ModelFactory,
            StatePaths: StatePaths,
            ModelStateRuntime: ModelStateRuntime,
            ManagerFacade: ManagerFacade,
            DeltaPayloadBuilder: DeltaPayloadBuilder,
            CreateSentinel: CreateSentinel,
            GatewayClient: GatewayClient,
            DebugLogger: DebugLogger,
            RuntimeTimerSanitizer: RuntimeTimerSanitizer,
            TimeConfigService: TimeConfigService,
            CtxFactory: CtxFactory,
            EffectApplier: EffectApplier,
            FeedbackPolicy: FeedbackPolicy,
            WorkflowCoordinator: WorkflowCoordinator,
            TelemetryRuntime: TelemetryRuntime,
            LayoutStateRuntime: LayoutStateRuntime,
            ActionDispatcher: ActionDispatcher,
            ActionContract: ActionContract,
            WorkflowTelemetry: WorkflowTelemetry,
            ThemeService: ThemeService,
            SearchUiConfig: SearchUiConfig,
            DetailFacade: DetailFacade,
            ApplyRuntimeSettingsUseCase: ApplyRuntimeSettingsUseCase,
            EnsureDictLoadedUseCase: EnsureDictLoadedUseCase,
            InitializeAppUseCase: InitializeAppUseCase,
            LoadCurrentUserUseCase: LoadCurrentUserUseCase,
            DiagnosticsUseCase: DiagnosticsUseCase
        });
        var mDeps = ComponentBootstrapDependencyBuilder.flatten(mGroups);
        mDeps.ComponentRuntimeSupport = ComponentAppRuntime.buildComponentRuntimeSupport();
        return {
            groups: mGroups,
            deps: mDeps
        };
    }

    function init(oComponent, aInitArgs) {
        var oBootstrap;
        var mBootstrapDeps;
        var oModelBootstrap;
        var oRuntimeContext;

        oComponent._oInteractionFX = InteractionFX;
        UIComponent.prototype.init.apply(oComponent, aInitArgs || []);
        oComponent._startupPerf = oComponent._startupPerf || {
            t0: (window.performance && typeof window.performance.now === "function") ? window.performance.now() : Date.now(),
            firstRouteReadyLogged: false,
            analyticsStartedLogged: false
        };
        ThemeService.syncDocumentRootClasses();

        oBootstrap = createBootstrapDeps(oComponent);
        mBootstrapDeps = oBootstrap.deps;

        oModelBootstrap = ComponentModelBootstrap.bootstrap(oComponent, mBootstrapDeps);
        DiagnosticsUseCase.execute({}, {
            mainServiceModel: oModelBootstrap.mainServiceModel,
            stateModel: oModelBootstrap.models.stateModel,
            getBackendMode: function () { return BackendModeContracts.MODES.REAL; },
            onMetadataFailed: function () {
                mBootstrapDeps.ModelStateRuntime.writeOnModel(oModelBootstrap.models.stateModel, BackendModeContracts.PATHS.BACKEND_MODE, BackendModeContracts.MODES.REAL);
            }
        });
        ComponentModelInitRuntime.registerModels(oComponent, oModelBootstrap.models);
        oRuntimeContext = ComponentCoreRuntimeBootstrap.bootstrap(oComponent, Object.assign({}, mBootstrapDeps, {
            bundleText: ComponentActionRuntime.createBundleText(oComponent),
            emitTelemetry: function (sEventName, oPayload) {
                return WorkflowTelemetry.emit(sEventName, {
                    stateModel: oModelBootstrap.models.stateModel,
                    payload: oPayload || {}
                });
            }
        }), Object.assign({}, oModelBootstrap.models, {
            mainServiceModel: oModelBootstrap.mainServiceModel
        }));

        return ComponentLifecycleBootstrap.bootstrap(oComponent, Object.assign({}, ComponentBootstrapDependencyBuilder.withManagerRuntime(mBootstrapDeps, ManagerFacade), {
            InitializeAppUseCase: InitializeAppUseCase,
            EnsureDictLoadedUseCase: EnsureDictLoadedUseCase,
            LoadCurrentUserUseCase: LoadCurrentUserUseCase
        }), Object.assign({}, oRuntimeContext, {
            models: Object.assign({}, oRuntimeContext.models, {
                mainServiceModel: oModelBootstrap.mainServiceModel
            })
        }));
    }

    return {
        init: init
    };
});
