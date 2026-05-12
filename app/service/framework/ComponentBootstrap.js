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
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackPolicy",
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
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxAdapterFactory",
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
    CtxAdapterFactory,
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

    var BOOTSTRAP_ERROR_CODES = Object.freeze({
        MISSING_MAIN_SERVICE_DATASOURCE: "bootstrap_main_service_datasource_missing",
        MISSING_MAIN_SERVICE_MODEL: "bootstrap_main_service_model_missing"
    });

    function initializeModelsStage(oComponent, mBootstrapDeps, oBootstrap) {
        var oModelBootstrap = bootstrapModels(oComponent, mBootstrapDeps);
        ModelStateRuntime.writeOnModel(
            oModelBootstrap.models.stateModel,
            BackendModeContracts.PATHS.BACKEND_MODE,
            oBootstrap.getBackendMode()
        );
        ComponentModelInitRuntime.registerModels(oComponent, oModelBootstrap.models);
        oComponent.setModel(oModelBootstrap.mainServiceModel);
        oComponent._detailFacade = oComponent._detailFacade || new DetailFacade();
        return oModelBootstrap;
    }

    function createMainServiceModel(oComponent, mDeps) {
        var oMainServiceModel = oComponent && oComponent.getModel ? oComponent.getModel("mainService") : null;
        var sManifestUri = oComponent && oComponent.getManifestEntry
            ? oComponent.getManifestEntry("/sap.app/dataSources/mainService/uri")
            : "";
        var sResolvedServiceUrl;

        if (!sManifestUri) {
            throw new Error(BOOTSTRAP_ERROR_CODES.MISSING_MAIN_SERVICE_DATASOURCE);
        }
        if (!oMainServiceModel) {
            throw new Error(BOOTSTRAP_ERROR_CODES.MISSING_MAIN_SERVICE_MODEL);
        }

        sResolvedServiceUrl = String((oMainServiceModel && oMainServiceModel.sServiceUrl) || sManifestUri || "").replace(/\/+$/, "");
        mDeps.GatewayClient.setModel(oMainServiceModel, { serviceUrl: sResolvedServiceUrl || sManifestUri });

        return oMainServiceModel;
    }

    function bootstrapModels(oComponent, mDeps) {
        var oBootstrapResult;

        oBootstrapResult = {
            models: mDeps.ComponentModelInitRuntime.initializeModels(oComponent, mDeps),
            mainServiceModel: createMainServiceModel(oComponent, mDeps)
        };
        oComponent._ctx = Object.assign({}, CtxAdapterFactory.buildInfraAdapters({
            state: oBootstrapResult.models.stateModel,
            masterData: oBootstrapResult.models.masterDataModel
        }, {
            uiState: null
        }));

        return oBootstrapResult;
    }

    function createBootstrapDeps() {
        var mDeps = {
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
            ComponentAutosaveRuntime: ComponentAutosaveRuntime,
            ComponentModelInitRuntime: ComponentModelInitRuntime,
            ComponentInitListenersRuntime: ComponentInitListenersRuntime,
            ComponentLockEventsRuntime: ComponentLockEventsRuntime,
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
        mDeps.managers = {
            GCDManager: GCDManager,
            ActivityMonitor: ActivityMonitor,
            AutoSaveCoordinator: AutoSaveCoordinator
        };
        mDeps.ComponentRuntimeSupport = ComponentAppRuntime.buildComponentRuntimeSupport();

        return {
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

    function initializeRuntimeServicesStage(oComponent, mBootstrapDeps, oModelBootstrap, mActionRuntimeOptions) {
        return mBootstrapDeps.ComponentRuntimeSettingsRuntime.initializeRuntimeSettings(oComponent, {
            stateModel: oModelBootstrap.models.stateModel,
            envState: oModelBootstrap.models.envState,
            masterDataModel: oModelBootstrap.models.masterDataModel,
            settingsManager: mBootstrapDeps.SettingsManager,
            gatewayBackendService: GatewayClient,
            telemetryRuntime: TelemetryRuntime,
            emitTelemetry: mActionRuntimeOptions.emitTelemetry
        });
    }

    function attachLifecycleStage(oComponent, mBootstrapDeps, mActionRuntimeOptions, mRuntimeModels) {
        return ComponentLifecycleRuntime.attachRuntime(
            oComponent,
            Object.assign({}, mBootstrapDeps, mActionRuntimeOptions),
            mRuntimeModels
        );
    }

    function init(oComponent, aInitArgs) {
        var oBootstrap;
        var mBootstrapDeps;
        var oModelBootstrap;
        var mRuntimeModels;
        var mActionRuntimeOptions;
        var oNavigationRuntime;
        var oRuntimeSettingsRuntime;

        initializeStartupState(oComponent, aInitArgs);

        oBootstrap = createBootstrapDeps();
        mBootstrapDeps = oBootstrap.deps;

        oModelBootstrap = initializeModelsStage(oComponent, mBootstrapDeps, oBootstrap);
        runDiagnostics(DiagnosticsUseCase, mBootstrapDeps, oModelBootstrap);
        mRuntimeModels = Object.assign({}, oModelBootstrap.models, {
            mainServiceModel: oModelBootstrap.mainServiceModel
        });
        mActionRuntimeOptions = {
            bundleText: ComponentFacadeEffectRuntime.createBundleText(oComponent),
            emitTelemetry: function (sEventName, oPayload) {
                return WorkflowTelemetry.emit(sEventName, {
                    stateModel: oModelBootstrap.models.stateModel,
                    payload: oPayload || {}
                });
            }
        };
        oRuntimeSettingsRuntime = initializeRuntimeServicesStage(oComponent, mBootstrapDeps, oModelBootstrap, mActionRuntimeOptions);
        oNavigationRuntime = attachLifecycleStage(oComponent, mBootstrapDeps, mActionRuntimeOptions, mRuntimeModels);

        return ComponentLifecycleRuntime.runBootSequence({
            component: oComponent,
            stateModel: oModelBootstrap.models.stateModel,
            shellModel: oModelBootstrap.models.shellModel,
            envState: oModelBootstrap.models.envState,
            cacheState: oModelBootstrap.models.cacheState,
            cacheAdapter: oComponent._ctx && oComponent._ctx.cache,
            initializeApp: function () {
                return InitializeAppUseCase.execute({}, { stateModel: oModelBootstrap.models.stateModel });
            },
            initializeAppUseCase: InitializeAppUseCase,
            ensureDictLoadedUseCase: EnsureDictLoadedUseCase,
            componentRuntimeSupport: mBootstrapDeps.ComponentRuntimeSupport,
            loadRuntimeSettings: function () {
                return oRuntimeSettingsRuntime && typeof oRuntimeSettingsRuntime.loadRuntimeSettings === "function"
                    ? oRuntimeSettingsRuntime.loadRuntimeSettings()
                    : Promise.resolve(null);
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
