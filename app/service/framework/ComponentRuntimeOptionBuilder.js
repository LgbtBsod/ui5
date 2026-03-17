sap.ui.define([], function () {
    "use strict";

    function buildManagerRuntimeOptions(mOptions) {
        return {
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            uiStateModel: mOptions.uiStateModel,
            snapshotModel: mOptions.snapshotModel,
            timerDefaults: mOptions.timerDefaults,
            managers: mOptions.managers,
            statePaths: mOptions.statePaths,
            deltaPayloadBuilder: mOptions.deltaPayloadBuilder,
            resolveDetailCurrent: mOptions.resolveDetailCurrent,
            applyFacadeResult: mOptions.applyFacadeResult,
            setGlobalBanner: mOptions.setGlobalBanner,
            emitTelemetry: mOptions.emitTelemetry,
            debugLogger: mOptions.debugLogger,
            actionContract: mOptions.actionContract,
            bundleText: mOptions.bundleText,
            componentRuntimeSupport: mOptions.componentRuntimeSupport,
            telemetryRuntime: mOptions.telemetryRuntime
        };
    }

    function buildLockRuntimeOptions(mOptions) {
        return {
            component: mOptions.component,
            mainServiceModel: mOptions.mainServiceModel,
            stateModel: mOptions.stateModel,
            uiStateModel: mOptions.uiStateModel,
            cacheModel: mOptions.cacheModel,
            statePaths: mOptions.statePaths,
            componentRuntimeSupport: mOptions.componentRuntimeSupport,
            timeConfigService: mOptions.timeConfigService,
            debugLogger: mOptions.debugLogger,
            bundleText: mOptions.bundleText,
            emitTelemetry: mOptions.emitTelemetry,
            setGlobalBanner: mOptions.setGlobalBanner,
            handleForceReadOnly: mOptions.handleForceReadOnly,
            applyFacadeResult: mOptions.applyFacadeResult,
            telemetryRuntime: mOptions.telemetryRuntime
        };
    }

    function buildListenerRuntimeOptions(mOptions) {
        return {
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            uiStateModel: mOptions.uiStateModel,
            selectedModel: mOptions.selectedModel,
            layoutModel: mOptions.layoutModel,
            cacheModel: mOptions.cacheModel,
            masterDataModel: mOptions.masterDataModel,
            envModel: mOptions.envModel,
            statePaths: mOptions.statePaths,
            smartSearchAdapter: mOptions.smartSearchAdapter,
            componentRuntimeSupport: mOptions.componentRuntimeSupport,
            timeConfigService: mOptions.timeConfigService,
            workflowCoordinator: mOptions.workflowCoordinator,
            bundleText: mOptions.bundleText,
            setGlobalBanner: mOptions.setGlobalBanner,
            clearGlobalBanner: mOptions.clearGlobalBanner,
            handleForceReadOnly: mOptions.handleForceReadOnly,
            runGuardedSave: mOptions.runGuardedSave,
            queuePendingNavigationIntent: mOptions.queuePendingNavigationIntent,
            clearPendingNavigationIntent: mOptions.clearPendingNavigationIntent,
            revertPendingNavigationIntent: mOptions.revertPendingNavigationIntent,
            resumePendingNavigationIntent: mOptions.resumePendingNavigationIntent,
            restorePendingNavigationIntent: mOptions.restorePendingNavigationIntent,
            emitTelemetry: mOptions.emitTelemetry,
            publishTabSignal: mOptions.publishTabSignal,
            telemetryRuntime: mOptions.telemetryRuntime,
            layoutStateRuntime: mOptions.layoutStateRuntime,
            actionContract: mOptions.actionContract
        };
    }

    return {
        buildListenerRuntimeOptions: buildListenerRuntimeOptions,
        buildLockRuntimeOptions: buildLockRuntimeOptions,
        buildManagerRuntimeOptions: buildManagerRuntimeOptions
    };
});
