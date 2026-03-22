sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFacadeEffectRuntime"
], function (
    ComponentFacadeEffectRuntime
) {
    "use strict";

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

    function buildLifecycleDeps(mDeps, oDependencyBuilder) {
        return Object.assign({}, oDependencyBuilder.withManagerRuntime(mDeps), {
            InitializeAppUseCase: mDeps.InitializeAppUseCase,
            EnsureDictLoadedUseCase: mDeps.EnsureDictLoadedUseCase,
            LoadCurrentUserUseCase: mDeps.LoadCurrentUserUseCase
        });
    }

    function buildLifecycleContext(oRuntimeContext, oRuntimeModels) {
        return Object.assign({}, oRuntimeContext, {
            models: Object.assign({}, oRuntimeContext.models, {
                mainServiceModel: oRuntimeModels.mainServiceModel
            })
        });
    }

    function buildTelemetryManagerOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport) {
        return {
            component: oComponent,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            detailModel: mModels.detailModel,
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
        };
    }

    function buildLockRuntimeOptions(oComponent, mDeps, mModels, mHandlers, mTelemetry, oRuntimeSupport) {
        return {
            component: oComponent,
            mainServiceModel: mModels.mainServiceModel,
            stateModel: mModels.stateModel,
            shellModel: mModels.shellModel,
            cacheState: mModels.cacheState,
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
            detailModel: mModels.detailModel,
            masterDataModel: mModels.masterDataModel,
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

    return {
        buildActionRuntimeOptions: buildActionRuntimeOptions,
        buildInitListenerOptions: buildInitListenerOptions,
        buildLifecycleContext: buildLifecycleContext,
        buildLifecycleDeps: buildLifecycleDeps,
        buildLockRuntimeOptions: buildLockRuntimeOptions,
        buildRuntimeModels: buildRuntimeModels,
        buildTelemetryManagerOptions: buildTelemetryManagerOptions
    };
});
