sap.ui.define([], function () {
    "use strict";

    function createAttachmentContext(oComponent, mDeps, mModels, mCore, mInitContext) {
        var oStateModel = mModels.stateModel;
        var oUiStateModel = mModels.uiStateModel;
        var oMainServiceModel = mModels.mainServiceModel;
        var StatePaths = mDeps.StatePaths;
        var oComponentRuntimeSupport = mDeps.ComponentRuntimeSupport || mDeps.componentRuntimeSupport;
        var oSaveGuardRuntime = mDeps.ComponentSaveGuardRuntime || mDeps.saveGuardRuntime;

    var fnHandleForceReadOnly = mDeps.ComponentRuntimeHandlerRuntime.createForceReadOnlyHandler({
            component: oComponent,
            stateModel: oStateModel,
            uiStateModel: oUiStateModel,
            statePaths: StatePaths,
            componentRuntimeSupport: oComponentRuntimeSupport,
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

    var fnRunGuardedSave = mDeps.ComponentRuntimeHandlerRuntime.createGuardedSave({
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
            saveGuardRuntime: oSaveGuardRuntime
        });

    var oCrossTabRuntime = mDeps.ComponentRuntimeHandlerRuntime.registerCrossTabHandlers({
            component: oComponent,
            stateModel: oStateModel,
            statePaths: StatePaths,
            bundleText: mCore.bundleText,
            setGlobalBanner: fnSetGlobalBanner,
            handleForceReadOnly: fnHandleForceReadOnly,
            attachCrossTabRuntime: mDeps.attachCrossTabRuntime
        }).crossTabRuntime;

    mDeps.ComponentRuntimeHandlerRuntime.registerDefaultHandlers({
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

    return {
        createAttachmentContext: createAttachmentContext
    };
});
