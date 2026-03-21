sap.ui.define([], function () {
    "use strict";

    function buildLockRuntimeOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport) {
        return {
            component: oComponent,
            mainServiceModel: mModels.mainServiceModel,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            cacheModel: mModels.cacheModel,
            statePaths: mDeps.StatePaths,
            componentRuntimeSupport: oRuntimeSupport,
            timeConfigService: mDeps.TimeConfigService,
            debugLogger: mDeps.DebugLogger,
            bundleText: mTelemetry.bundleText,
            emitTelemetry: mTelemetry.emitTelemetry,
            setGlobalBanner: mHandlers.setGlobalBanner,
            handleForceReadOnly: mHandlers.handleForceReadOnly,
            applyFacadeResult: mHandlers.applyFacadeResult,
            telemetryRuntime: mDeps.TelemetryRuntime
        };
    }

    function buildInitListenerOptions(oComponent, mDeps, mModels, mHandlers, mServices, mTelemetry, oRuntimeSupport) {
        return {
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            selectedModel: mModels.selectedModel,
            cacheModel: mModels.cacheModel,
            masterDataModel: mModels.masterDataModel,
            envModel: mModels.envModel,
            statePaths: mDeps.StatePaths,
            searchConfig: mServices.searchConfig,
            componentRuntimeSupport: oRuntimeSupport,
            timeConfigService: mDeps.TimeConfigService,
            workflowCoordinator: mDeps.WorkflowCoordinator,
            bundleText: mTelemetry.bundleText,
            setGlobalBanner: mHandlers.setGlobalBanner,
            clearGlobalBanner: mHandlers.clearGlobalBanner,
            handleForceReadOnly: mHandlers.handleForceReadOnly,
            runGuardedSave: mHandlers.runGuardedSave,
            queuePendingNavigationIntent: mHandlers.queuePendingNavigationIntent,
            clearPendingNavigationIntent: mHandlers.clearPendingNavigationIntent,
            revertPendingNavigationIntent: mHandlers.revertPendingNavigationIntent,
            resumePendingNavigationIntent: mHandlers.resumePendingNavigationIntent,
            restorePendingNavigationIntent: mHandlers.restorePendingNavigationIntent,
            emitTelemetry: mTelemetry.emitTelemetry,
            publishTabSignal: mHandlers.publishTabSignal,
            telemetryRuntime: mDeps.TelemetryRuntime,
            layoutStateRuntime: mDeps.LayoutStateRuntime,
            actionContract: mDeps.ActionContract
        };
    }

    function initializeRouter(oComponent) {
        var oRouter = oComponent && oComponent.getRouter && oComponent.getRouter();
        if (!oRouter || typeof oRouter.initialize !== "function" || oComponent._routerInitialized) {
            return;
        }
        oRouter.initialize();
        oComponent._routerInitialized = true;
    }

    function bootstrap(oComponent, mDeps, oRuntimeContext) {
        var mModels = oRuntimeContext.models;
        var mHandlers = oRuntimeContext.handlers;
        var mServices = oRuntimeContext.services;
        var mTelemetry = oRuntimeContext.telemetry;
        var oRuntimeSupport = mServices.componentRuntimeSupport;

        mDeps.ComponentManagerOrchestrationRuntime.attachManagerRuntime({
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            snapshotModel: mModels.snapshotModel,
            timerDefaults: mTelemetry.timerDefaults,
            managers: mDeps.managers,
            statePaths: mDeps.StatePaths,
            deltaPayloadBuilder: mDeps.DeltaPayloadBuilder,
            buildLatestCtx: mHandlers.buildLatestCtx,
            resolveDetailCurrent: mHandlers.resolveDetailCurrent,
            applyFacadeResult: mHandlers.applyFacadeResult,
            setGlobalBanner: mHandlers.setGlobalBanner,
            emitTelemetry: mTelemetry.emitTelemetry,
            debugLogger: mDeps.DebugLogger,
            actionContract: mDeps.ActionContract,
            bundleText: mTelemetry.bundleText,
            componentRuntimeSupport: oRuntimeSupport,
            telemetryRuntime: mDeps.TelemetryRuntime
        });
        mDeps.ComponentLockEventsRuntime.attachLockRuntime(
            buildLockRuntimeOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport)
        );
        mDeps.ComponentInitListenersRuntime.attachInitListeners(
            buildInitListenerOptions(oComponent, mDeps, mModels, mHandlers, mServices, mTelemetry, oRuntimeSupport)
        );

        initializeRouter(oComponent);

        return mDeps.ComponentBootRuntime.runBootSequence({
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            envModel: mModels.envModel,
            cacheModel: mModels.cacheModel,
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
