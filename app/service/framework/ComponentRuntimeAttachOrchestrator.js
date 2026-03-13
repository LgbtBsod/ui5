sap.ui.define([], function () {
    "use strict";

    function attachRuntimeStages(oComponent, mDeps, mModels, mCore, mAttachments) {
        var oComponentRuntimeSupport = mDeps.ComponentRuntimeSupport || mDeps.componentRuntimeSupport;
        mDeps.attachManagerRuntime(mDeps.ComponentRuntimeOptionBuilder.buildManagerRuntimeOptions({
            component: oComponent,
            stateModel: mModels.stateModel,
            uiStateModel: mModels.uiStateModel,
            snapshotModel: mModels.snapshotModel,
            timerDefaults: mCore.timerDefaults,
            managers: mDeps.managers,
            statePaths: mDeps.StatePaths,
            deltaPayloadBuilder: mDeps.DeltaPayloadBuilder,
            resolveDetailCurrent: mCore.resolveDetailCurrent,
            applyFacadeResult: mCore.applyFacadeResult,
            setGlobalBanner: mAttachments.setGlobalBanner,
            emitTelemetry: mCore.emitTelemetry,
            debugLogger: mDeps.DebugLogger,
            actionContract: mDeps.ActionContract,
            bundleText: mCore.bundleText,
            componentRuntimeSupport: oComponentRuntimeSupport,
            telemetryRuntime: mDeps.TelemetryRuntime
        }));
        mDeps.attachLockRuntime(mDeps.ComponentRuntimeOptionBuilder.buildLockRuntimeOptions({
            component: oComponent,
            mainServiceModel: mModels.mainServiceModel,
            stateModel: mModels.stateModel,
            uiStateModel: mModels.uiStateModel,
            cacheModel: mModels.cacheModel,
            statePaths: mDeps.StatePaths,
            componentRuntimeSupport: oComponentRuntimeSupport,
            timeConfigService: mDeps.TimeConfigService,
            debugLogger: mDeps.DebugLogger,
            bundleText: mCore.bundleText,
            emitTelemetry: mCore.emitTelemetry,
            setGlobalBanner: mAttachments.setGlobalBanner,
            handleForceReadOnly: mAttachments.handleForceReadOnly,
            applyFacadeResult: mCore.applyFacadeResult,
            telemetryRuntime: mDeps.TelemetryRuntime
        }));
        mDeps.attachInitListeners(mDeps.ComponentRuntimeOptionBuilder.buildListenerRuntimeOptions({
            component: oComponent,
            stateModel: mModels.stateModel,
            uiStateModel: mModels.uiStateModel,
            selectedModel: mModels.selectedModel,
            layoutModel: mModels.layoutModel,
            cacheModel: mModels.cacheModel,
            masterDataModel: mModels.masterDataModel,
            envModel: mModels.envModel,
            statePaths: mDeps.StatePaths,
            smartSearchAdapter: mDeps.SmartSearchAdapter,
            componentRuntimeSupport: oComponentRuntimeSupport,
            timeConfigService: mDeps.TimeConfigService,
            flowCoordinator: mDeps.FlowCoordinator,
            bundleText: mCore.bundleText,
            setGlobalBanner: mAttachments.setGlobalBanner,
            clearGlobalBanner: mAttachments.clearGlobalBanner,
            handleForceReadOnly: mAttachments.handleForceReadOnly,
            runGuardedSave: mAttachments.runGuardedSave,
            queuePendingNavigationIntent: mAttachments.queuePendingNavigationIntent,
            clearPendingNavigationIntent: mAttachments.clearPendingNavigationIntent,
            revertPendingNavigationIntent: mAttachments.revertPendingNavigationIntent,
            resumePendingNavigationIntent: mAttachments.resumePendingNavigationIntent,
            restorePendingNavigationIntent: mAttachments.restorePendingNavigationIntent,
            emitTelemetry: mCore.emitTelemetry,
            publishTabSignal: mAttachments.publishTabSignal,
            telemetryRuntime: mDeps.TelemetryRuntime,
            layoutStateRuntime: mDeps.LayoutStateRuntime,
            actionContract: mDeps.ActionContract
        }));
    }

    return {
        attachRuntimeStages: attachRuntimeStages
    };
});
