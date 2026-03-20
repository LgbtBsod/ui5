sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapContracts",
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
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockEventsRuntime"
], function (
    ComponentBootstrapContracts,
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
    ComponentLockEventsRuntime
) {
    "use strict";

    var GROUPS = ComponentBootstrapContracts.GROUPS;
    var MANAGER_KEYS = ComponentBootstrapContracts.MANAGER_KEYS;

    function build(mStaticDeps) {
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
                CtxFactory: mStaticDeps.CtxFactory,
                EffectApplier: mStaticDeps.EffectApplier,
                FeedbackPolicy: mStaticDeps.FeedbackPolicy,
                WorkflowCoordinator: mStaticDeps.WorkflowCoordinator,
                TelemetryRuntime: mStaticDeps.TelemetryRuntime,
                LayoutStateRuntime: mStaticDeps.LayoutStateRuntime,
                ActionDispatcher: mStaticDeps.ActionDispatcher,
                ActionContract: mStaticDeps.ActionContract,
                WorkflowTelemetry: mStaticDeps.WorkflowTelemetry,
                CreateSentinel: mStaticDeps.CreateSentinel,
                StatePaths: mStaticDeps.StatePaths,
                SearchUiConfig: mStaticDeps.SearchUiConfig,
                DetailFacade: mStaticDeps.DetailFacade
            },
            managers: {
                Managers: mStaticDeps.ManagerFacade,
                managers: {}
            },
            runtime: {
                ComponentLockReleaseRuntime: ComponentLockReleaseRuntime,
                ComponentSaveGuardRuntime: ComponentSaveGuardRuntime,
                ComponentModelInitRuntime: ComponentModelInitRuntime,
                ComponentMainServiceRuntime: ComponentMainServiceRuntime,
                ComponentCoreInitRuntime: ComponentCoreInitRuntime,
                ComponentStateSeedRuntime: ComponentStateSeedRuntime,
                ComponentActionRuntime: ComponentActionRuntime,
                ComponentFeedbackInitRuntime: ComponentFeedbackInitRuntime,
                ComponentRuntimeHandlerRuntime: ComponentRuntimeHandlerRuntime,
                ComponentCrossTabRuntime: ComponentCrossTabRuntime,
                ComponentInitListenersRuntime: ComponentInitListenersRuntime,
                ComponentManagerOrchestrationRuntime: ComponentManagerOrchestrationRuntime,
                ComponentLockEventsRuntime: ComponentLockEventsRuntime,
                ComponentModelBootstrap: ComponentModelBootstrap,
                ComponentCoreRuntimeBootstrap: ComponentCoreRuntimeBootstrap,
                ComponentLifecycleBootstrap: ComponentLifecycleBootstrap,
                ComponentBootRuntime: ComponentBootRuntime
            },
            theme: {
                ThemeRuntime: mStaticDeps.ThemeService
            },
            usecases: {
                ApplyRuntimeSettingsUseCase: mStaticDeps.ApplyRuntimeSettingsUseCase,
                EnsureDictLoadedUseCase: mStaticDeps.EnsureDictLoadedUseCase,
                InitializeAppUseCase: mStaticDeps.InitializeAppUseCase,
                LoadCurrentUserUseCase: mStaticDeps.LoadCurrentUserUseCase,
                DiagnosticsUseCase: mStaticDeps.DiagnosticsUseCase
            }
        };
    }

    function withManagerRuntime(mDeps, oManagerFacade) {
        var mResolved = Object.assign({}, mDeps);
        mResolved.managers = {};
        mResolved.managers[MANAGER_KEYS.HEARTBEAT_MANAGER] = oManagerFacade.HeartbeatManager;
        mResolved.managers[MANAGER_KEYS.GCD_MANAGER] = oManagerFacade.GCDManager;
        mResolved.managers[MANAGER_KEYS.ACTIVITY_MONITOR] = oManagerFacade.ActivityMonitor;
        mResolved.managers[MANAGER_KEYS.AUTOSAVE_COORDINATOR] = oManagerFacade.AutoSaveCoordinator;
        mResolved.managers[MANAGER_KEYS.LOCK_STATUS_MONITOR] = oManagerFacade.LockStatusMonitor;
        return mResolved;
    }

    function flatten(mGroups) {
        return Object.assign({},
            mGroups[GROUPS.CORE.toLowerCase()] || mGroups.core || {},
            mGroups[GROUPS.MANAGERS.toLowerCase()] || mGroups.managers || {},
            mGroups[GROUPS.RUNTIME.toLowerCase()] || mGroups.runtime || {},
            mGroups[GROUPS.THEME.toLowerCase()] || mGroups.theme || {},
            mGroups[GROUPS.USECASES.toLowerCase()] || mGroups.usecases || {}
        );
    }

    return {
        build: build,
        flatten: flatten,
        withManagerRuntime: withManagerRuntime
    };
});
