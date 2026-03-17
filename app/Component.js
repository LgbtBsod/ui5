sap.ui.define([
    "sap/ui/core/UIComponent",
    "PRODUCTION_CONTROL_CHECKLIST/model/ModelFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/SmartSearchAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/ManagerFacade",
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowCoordinator",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/DeltaPayloadBuilder",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayBackendService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeTimerSanitizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/ApplyRuntimeSettingsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/EnsureDictLoadedUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/InitializeAppUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/StartManagersUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/DiagnosticsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/CtxFactory",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectApplier",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLockReleaseRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentSaveGuardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentModelInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentMainServiceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentCoreInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentStateSeedRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailFacade",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionDispatcher",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "sap/ui/model/odata/v2/ODataModel",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
"PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "sap/ui/Device",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/InteractionFX",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ThemeService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAppRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime"
], function (
    UIComponent,
    ModelFactory,
    SmartSearchAdapter,
    ManagerFacade,
    JSONModel,
    WorkflowCoordinator,
    DeltaPayloadBuilder,
    GatewayBackendService,
    DebugLogger,
    RuntimeTimerSanitizer,
    TimeConfigService,
    ApplyRuntimeSettingsUseCase,
    EnsureDictLoadedUseCase,
    InitializeAppUseCase,
    LoadCurrentUserUseCase,
    StartManagersUseCase,
    DiagnosticsUseCase,
    CtxFactory,
    EffectApplier,
    FeedbackPolicy,
    ComponentLockReleaseRuntime,
    ComponentSaveGuardRuntime,
    ComponentInitRuntime,
    ComponentModelInitRuntime,
    ComponentMainServiceRuntime,
    ComponentCoreInitRuntime,
    ComponentStateSeedRuntime,
    TelemetryRuntime,
    LayoutStateRuntime,
    StatePaths,
    DetailFacade,
    ActionDispatcher,
    ActionContract,
    ODataModel,
    WorkflowTelemetry,
    CreateSentinel,
    Device,
    InteractionFX,
    ThemeService,
    ComponentAppRuntime,
    EditSessionRuntime
) {
    "use strict";

    // Polyfill: Promise.prototype.finally (not available in IE11 / older SAP WebAS Chromium)
    if (typeof Promise === "function" && typeof Promise.prototype.finally !== "function") {
        Promise.prototype.finally = function (fnCallback) {
            var P = this.constructor || Promise;
            return this.then(
                function (vValue) { return P.resolve(typeof fnCallback === "function" ? fnCallback() : undefined).then(function () { return vValue; }); },
                function (oReason) { return P.resolve(typeof fnCallback === "function" ? fnCallback() : undefined).then(function () { throw oReason; }); }
            );
        };
    }


    return UIComponent.extend("PRODUCTION_CONTROL_CHECKLIST.Component", {
        metadata: {
            manifest: "json"
        },
        init: function () {
            return ComponentInitRuntime.runInit.call(this, arguments, {
                UIComponent: UIComponent,
                ModelFactory: ModelFactory,
                SmartSearchAdapter: SmartSearchAdapter,
                Managers: ManagerFacade,
                JSONModel: JSONModel,
        FlowCoordinator: WorkflowCoordinator,
                DeltaPayloadBuilder: DeltaPayloadBuilder,
                GatewayBackendService: GatewayBackendService,
                DebugLogger: DebugLogger,
                RuntimeTimerSanitizer: RuntimeTimerSanitizer,
                TimeConfigService: TimeConfigService,
                ApplyRuntimeSettingsUseCase: ApplyRuntimeSettingsUseCase,
                EnsureDictLoadedUseCase: EnsureDictLoadedUseCase,
                InitializeAppUseCase: InitializeAppUseCase,
                LoadCurrentUserUseCase: LoadCurrentUserUseCase,
                StartManagersUseCase: StartManagersUseCase,
                DiagnosticsUseCase: DiagnosticsUseCase,
                CtxFactory: CtxFactory,
                EffectApplier: EffectApplier,
                FeedbackPolicy: FeedbackPolicy,
                ComponentRuntimeSupport: ComponentAppRuntime.buildComponentRuntimeSupport(),
                ComponentLockReleaseRuntime: ComponentLockReleaseRuntime,
                ComponentSaveGuardRuntime: ComponentSaveGuardRuntime,
                ComponentModelInitRuntime: ComponentModelInitRuntime,
                ComponentMainServiceRuntime: ComponentMainServiceRuntime,
                ComponentCoreInitRuntime: ComponentCoreInitRuntime,
                ComponentStateSeedRuntime: ComponentStateSeedRuntime,
                TelemetryRuntime: TelemetryRuntime,
                LayoutStateRuntime: LayoutStateRuntime,
                StatePaths: StatePaths,
                DetailFacade: DetailFacade,
                ActionDispatcher: ActionDispatcher,
                ActionContract: ActionContract,
                ODataModel: ODataModel,
                WorkflowTelemetry: WorkflowTelemetry,
                CreateSentinel: CreateSentinel,
                Device: Device,
                InteractionFX: InteractionFX,
                ThemeRuntime: ThemeService
            });
        },
        _startCoreManagers: function () {
            return StartManagersUseCase.execute({ scope: "core" }, { managers: this._collectManagers() });
        },
        _stopLockScopedManagers: function () {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: false }, { managers: this._collectManagers() });
        },
        _startLockScopedManagers: function () {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: true }, { managers: this._collectManagers() });
        },
        _stopAllManagers: function () {
            EditSessionRuntime.stopAll(this._collectManagers());
        },
        _collectManagers: function () {
            return ComponentAppRuntime.collectManagers(this);
        },
        _isLockRuntimeActive: function (oStateModel) {
            return ComponentAppRuntime.isLockRuntimeActive(oStateModel);
        },
        _syncLockScopedManagers: function (oStateModel) {
            return ComponentAppRuntime.syncLockScopedManagers(this, oStateModel);
        },
        _applyFrontendRuntimeConfig: function (oFrontendConfig, oStateModel, oEnvModel, oMasterDataModel) {
            return ComponentAppRuntime.applyFrontendRuntimeConfig(this, oFrontendConfig, oStateModel, oEnvModel, oMasterDataModel, ApplyRuntimeSettingsUseCase);
        },
        _applyManagersTimerConfig: function (mTimers) {
            return ComponentAppRuntime.applyManagersTimerConfig(this, mTimers);
        },
        _registerLockReleaseBeacon: function (oStateModel, oMainServiceModel) {
            return ComponentAppRuntime.registerLockReleaseBeacon(this, oStateModel, oMainServiceModel);
        },
        _releaseActiveLockOnLeave: function (oStateModel, oMainServiceModel) {
            return ComponentAppRuntime.releaseActiveLockOnLeave(this, oStateModel, oMainServiceModel);
        },
        attachInteractionFxToApp: function (oDomRef) {
            if (this._oInteractionFxHandle && this._oInteractionFxHandle.destroy) {
                this._oInteractionFxHandle.destroy();
                this._oInteractionFxHandle = null;
            }
            if (this._oInteractionFX && typeof this._oInteractionFX.attach === "function") {
                this._oInteractionFxHandle = this._oInteractionFX.attach(oDomRef);
            }
        },
        exit: function () {
            ComponentAppRuntime.destroyComponentRuntime(this);
        }
    });
});

