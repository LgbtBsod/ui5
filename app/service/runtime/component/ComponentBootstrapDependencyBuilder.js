sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentAutosaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockReleaseRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSaveGuardRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentModelInitRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentMainServiceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentModelBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLifecycleRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentRuntimeSettingsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentPollingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentCrossTabRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentInitListenersRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockEventsRuntime"
], function (
    ComponentBootstrapContracts,
    ComponentAutosaveRuntime,
    ComponentLockReleaseRuntime,
    ComponentSaveGuardRuntime,
    ComponentModelInitRuntime,
    ComponentMainServiceRuntime,
    ComponentModelBootstrap,
    ComponentLifecycleRuntime,
    ComponentRuntimeSettingsRuntime,
    ComponentPollingRuntime,
    ComponentCrossTabRuntime,
    ComponentInitListenersRuntime,
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
                ComponentMainServiceRuntime: ComponentMainServiceRuntime,
                ComponentRuntimeSettingsRuntime: ComponentRuntimeSettingsRuntime,
                ComponentPollingRuntime: ComponentPollingRuntime,
                ComponentCrossTabRuntime: ComponentCrossTabRuntime,
                ComponentInitListenersRuntime: ComponentInitListenersRuntime,
                ComponentLockEventsRuntime: ComponentLockEventsRuntime,
                ComponentModelBootstrap: ComponentModelBootstrap,
                ComponentLifecycleRuntime: ComponentLifecycleRuntime,
                ComponentNavigationRuntime: mStaticDeps.ComponentNavigationRuntime
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

    function withManagerRuntime(mDeps) {
        var mResolved = Object.assign({}, mDeps);
        var oManagers = (mResolved.Managers || (mResolved.groups && mResolved.groups.managers && mResolved.groups.managers.Managers)) || {};
        mResolved.managers = {};
        mResolved.managers[MANAGER_KEYS.GCD_MANAGER] = oManagers.GCDManager;
        mResolved.managers[MANAGER_KEYS.ACTIVITY_MONITOR] = oManagers.ActivityMonitor;
        mResolved.managers[MANAGER_KEYS.AUTOSAVE_COORDINATOR] = oManagers.AutoSaveCoordinator;
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
