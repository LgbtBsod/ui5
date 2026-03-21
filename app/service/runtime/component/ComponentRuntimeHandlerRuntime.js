sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime"
], function (EditSessionRuntime, ShellStateRuntime) {
    "use strict";

    function createForceReadOnlyHandler(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oShellModel = mOptions.shellModel;
        var TelemetryRuntime = mOptions.telemetryRuntime;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnClearPendingNavigationIntent = mOptions.clearPendingNavigationIntent;

        return function (mInput) {
            var mForceInput = Object.assign({}, mInput || {});
            if (!Object.prototype.hasOwnProperty.call(mForceInput, "preserveDirty")) {
                mForceInput.preserveDirty = false;
            }
            EditSessionRuntime.stopLockScoped(oComponent._collectManagers());
            return oComponent._detailFacade.forceReadOnly(mForceInput, oComponent._ctx).then(function (oResult) {
                if (typeof fnClearPendingNavigationIntent === "function") {
                    fnClearPendingNavigationIntent();
                }
                fnApplyFacadeResult(oResult);
                ShellStateRuntime.syncRuntimeShellState(oStateModel, oShellModel);
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
