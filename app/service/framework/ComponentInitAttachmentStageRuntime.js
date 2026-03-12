sap.ui.define([], function () {
    "use strict";

    function createAttachmentContext(oComponent, mDeps, mModels, mCore, mInitContext) {
        var oStateModel = mModels.stateModel;
        var oUiStateModel = mModels.uiStateModel;
        var oCacheModel = mModels.cacheModel;
        var oSelectedModel = mModels.selectedModel;
        var oLayoutModel = mModels.layoutModel;
        var oMasterDataModel = mModels.masterDataModel;
        var oEnvModel = mModels.envModel;
        var oMainServiceModel = mModels.mainServiceModel;
        var StatePaths = mDeps.StatePaths;

        var fnHandleForceReadOnly = mDeps.ComponentRuntimeHandlerBootstrap.createForceReadOnlyHandler({
            component: oComponent,
            stateModel: oStateModel,
            uiStateModel: oUiStateModel,
            statePaths: StatePaths,
            componentRuntimeSupport: mDeps.ComponentRuntimeSupport,
            telemetryRuntime: mDeps.TelemetryRuntime,
            applyFacadeResult: mCore.applyFacadeResult,
            emitTelemetry: mCore.emitTelemetry,
            readDirty: function () {
                return mDeps.ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
            }
        });

        var fnResolveCorrelationId = mInitContext.feedbackRuntime.resolveCorrelationId;
        var fnIsSessionExpiredError = mInitContext.feedbackRuntime.isSessionExpiredError;
        var fnSetGlobalBanner = mInitContext.feedbackRuntime.setGlobalBanner;
        var fnClearGlobalBanner = mInitContext.feedbackRuntime.clearGlobalBanner;
        var oPendingNavigationRuntime = mInitContext.pendingNavigationRuntime;

        var fnRunGuardedSave = mDeps.ComponentRuntimeHandlerBootstrap.createGuardedSave({
            component: oComponent,
            stateModel: oStateModel,
            mainServiceModel: oMainServiceModel,
            statePaths: StatePaths,
            buildLatestCtx: mCore.buildLatestCtx,
            applyFacadeResult: mCore.applyFacadeResult,
            emitTelemetry: mCore.emitTelemetry,
            resumePendingNavigationIntent: oPendingNavigationRuntime.resumePendingNavigationIntent,
            resolveCorrelationId: fnResolveCorrelationId,
            isSessionExpiredError: fnIsSessionExpiredError,
            setGlobalBanner: fnSetGlobalBanner,
            clearGlobalBanner: fnClearGlobalBanner,
            saveGuardSupport: mDeps.ComponentInitSaveGuardSupport
        });

        var oCrossTabRuntime = mDeps.ComponentRuntimeHandlerBootstrap.registerCrossTabRuntime({
            component: oComponent,
            stateModel: oStateModel,
            statePaths: StatePaths,
            bundleText: mCore.bundleText,
            setGlobalBanner: fnSetGlobalBanner,
            handleForceReadOnly: fnHandleForceReadOnly,
            attachCrossTabRuntime: mDeps.attachCrossTabRuntime
        }).crossTabRuntime;

        mDeps.ComponentRuntimeHandlerBootstrap.registerDefaultHandlers({
            component: oComponent,
            actionContract: mDeps.ActionContract,
            runGuardedSave: fnRunGuardedSave,
            buildLatestCtx: mCore.buildLatestCtx,
            applyFacadeResult: mCore.applyFacadeResult,
            registerDefaultHandlers: mDeps.registerDefaultHandlers
        });

        return {
            clearPendingNavigationIntent: oPendingNavigationRuntime.clearPendingNavigationIntent,
            clearGlobalBanner: fnClearGlobalBanner,
            crossTabRuntime: oCrossTabRuntime,
            handleForceReadOnly: fnHandleForceReadOnly,
            publishTabSignal: oCrossTabRuntime.publishTabSignal,
            queuePendingNavigationIntent: oPendingNavigationRuntime.queuePendingNavigationIntent,
            restorePendingNavigationIntent: oPendingNavigationRuntime.restorePendingNavigationIntent,
            resumePendingNavigationIntent: oPendingNavigationRuntime.resumePendingNavigationIntent,
            revertPendingNavigationIntent: oPendingNavigationRuntime.revertPendingNavigationIntent,
            runGuardedSave: fnRunGuardedSave,
            runtimeSettingsRuntime: mInitContext.runtimeSettingsRuntime,
            setGlobalBanner: fnSetGlobalBanner
        };
    }

    function attachRuntimeStages(oComponent, mDeps, mModels, mCore, mAttachments) {
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
            componentRuntimeSupport: mDeps.ComponentRuntimeSupport,
            telemetryRuntime: mDeps.TelemetryRuntime
        }));
        mDeps.attachLockRuntime(mDeps.ComponentRuntimeOptionBuilder.buildLockRuntimeOptions({
            component: oComponent,
            mainServiceModel: mModels.mainServiceModel,
            stateModel: mModels.stateModel,
            uiStateModel: mModels.uiStateModel,
            cacheModel: mModels.cacheModel,
            statePaths: mDeps.StatePaths,
            componentRuntimeSupport: mDeps.ComponentRuntimeSupport,
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
            componentRuntimeSupport: mDeps.ComponentRuntimeSupport,
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
        attachRuntimeStages: attachRuntimeStages,
        createAttachmentContext: createAttachmentContext
    };
});
