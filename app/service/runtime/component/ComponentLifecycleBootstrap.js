sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentRuntimeOptionsFactory"
], function (
    ComponentRuntimeOptionsFactory
) {
    "use strict";

    function initializeRouter(oComponent) {
        var oRouter = oComponent && oComponent.getRouter && oComponent.getRouter();
        if (!oRouter || typeof oRouter.initialize !== "function" || oComponent._routerInitialized) {
            return;
        }
        oRouter.initialize();
        oComponent._routerInitialized = true;
    }

    function attachPollingAndAutosave(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport) {
        var mTelemetryManagerOptions = ComponentRuntimeOptionsFactory.buildTelemetryManagerOptions(
            oComponent,
            mDeps,
            mModels,
            mHandlers,
            mTelemetry,
            oRuntimeSupport
        );

        mDeps.ComponentPollingRuntime.createHeartbeatManager(mTelemetryManagerOptions);
        mDeps.ComponentPollingRuntime.createSupportManagers({
            component: oComponent,
            timerDefaults: mTelemetry.timerDefaults,
            managers: mDeps.managers
        });
        mDeps.ComponentAutosaveRuntime.createAutoSaveManager(mTelemetryManagerOptions);
        mDeps.ComponentPollingRuntime.createLockStatusManager(mTelemetryManagerOptions);
    }

    function bootstrap(oComponent, mDeps, oRuntimeContext) {
        var mModels = oRuntimeContext.models;
        var mHandlers = oRuntimeContext.handlers;
        var mServices = oRuntimeContext.services;
        var mTelemetry = oRuntimeContext.telemetry;
        var oRuntimeSupport = mServices.componentRuntimeSupport;

        attachPollingAndAutosave(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport);
        mDeps.ComponentLockEventsRuntime.attachLockRuntime(
            ComponentRuntimeOptionsFactory.buildLockRuntimeOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport)
        );
        mDeps.ComponentInitListenersRuntime.attachInitListeners(
            ComponentRuntimeOptionsFactory.buildInitListenerOptions(oComponent, mDeps, mModels, mHandlers, mServices, mTelemetry, oRuntimeSupport)
        );

        initializeRouter(oComponent);

        return mDeps.ComponentBootRuntime.runBootSequence({
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            envState: mModels.envState,
            cacheState: mModels.cacheState,
            cacheAdapter: oComponent._ctx && oComponent._ctx.cache,
            initializeAppUseCase: mDeps.InitializeAppUseCase,
            ensureDictLoadedUseCase: mDeps.EnsureDictLoadedUseCase,
            componentRuntimeSupport: oRuntimeSupport,
            loadRuntimeSettings: mServices.runtimeSettingsRuntime.loadRuntimeSettings,
            loadCurrentUser: function () {
                return mDeps.LoadCurrentUserUseCase && mDeps.LoadCurrentUserUseCase.refresh
                    ? mDeps.LoadCurrentUserUseCase.refresh({ stateModel: mModels.stateModel })
                    : Promise.resolve(null);
            },
            bundleText: mTelemetry.bundleText
        });
    }

    return {
        bootstrap: bootstrap
    };
});
