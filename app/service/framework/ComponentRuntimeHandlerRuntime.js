sap.ui.define([], function () {
    "use strict";

    function createForceReadOnlyHandler(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var TelemetryRuntime = mOptions.telemetryRuntime;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnEmitTelemetry = mOptions.emitTelemetry;

        return function (mInput) {
            var mForceInput = Object.assign({}, mInput || {});
            if (!Object.prototype.hasOwnProperty.call(mForceInput, "preserveDirty")) {
                mForceInput.preserveDirty = !!mOptions.readDirty();
            }
            oComponent._oHeartbeat.stop();
            oComponent._oLockStatus.stop();
            oComponent._oAutoSave.stop();
            oComponent._oGcd.destroyManager();
            return oComponent._detailFacade.forceReadOnly(mForceInput, oComponent._ctx).then(function (oResult) {
                fnApplyFacadeResult(oResult);
                ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
                fnEmitTelemetry("lock.lost.detected", TelemetryRuntime.lockLost(
                    mForceInput && mForceInput.reason,
                    mForceInput && mForceInput.source
                ));
                return oResult;
            });
        };
    }

    function createGuardedSave(mOptions) {
        var oSaveGuardRuntime = mOptions.saveGuardRuntime || mOptions.saveGuardSupport;
        return oSaveGuardRuntime.createRunGuardedSave({
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            mainServiceModel: mOptions.mainServiceModel,
            statePaths: mOptions.statePaths,
            detailFacade: mOptions.component._detailFacade,
            buildLatestCtx: mOptions.buildLatestCtx,
            applyFacadeResult: mOptions.applyFacadeResult,
            emitTelemetry: mOptions.emitTelemetry,
            resumePendingNavigationIntent: mOptions.resumePendingNavigationIntent,
            resolveCorrelationId: mOptions.resolveCorrelationId,
            isSessionExpiredError: mOptions.isSessionExpiredError,
            setGlobalBanner: mOptions.setGlobalBanner,
            clearGlobalBanner: mOptions.clearGlobalBanner
        });
    }

    function registerCrossTabHandlers(mOptions) {
        var oCrossTabRuntime = mOptions.attachCrossTabRuntime({
            component: mOptions.component,
            stateModel: mOptions.stateModel,
            statePaths: mOptions.statePaths,
            bundleText: mOptions.bundleText,
            setGlobalBanner: mOptions.setGlobalBanner,
            handleForceReadOnly: mOptions.handleForceReadOnly
        });
        return {
            crossTabRuntime: oCrossTabRuntime,
            publishTabSignal: oCrossTabRuntime.publishTabSignal
        };
    }

    function registerDefaultHandlers(mOptions) {
        mOptions.registerDefaultHandlers({
            actionDispatcher: mOptions.component._actionDispatcher,
            actionContract: mOptions.actionContract,
            detailFacade: mOptions.component._detailFacade,
            runGuardedSave: mOptions.runGuardedSave,
            buildLatestCtx: mOptions.buildLatestCtx,
            applyFacadeResult: mOptions.applyFacadeResult,
            getCtx: function () { return mOptions.component._ctx; }
        });
    }

    return {
        createForceReadOnlyHandler: createForceReadOnlyHandler,
        createGuardedSave: createGuardedSave,
        registerCrossTabHandlers: registerCrossTabHandlers,
        registerDefaultHandlers: registerDefaultHandlers
    };
});
