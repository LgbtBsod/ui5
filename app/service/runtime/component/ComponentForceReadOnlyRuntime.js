sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime"
], function (
    EditSessionRuntime,
    ShellStateRuntime
) {
    "use strict";

    function createHandler(mOptions) {
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

    return {
        createHandler: createHandler
    };
});
