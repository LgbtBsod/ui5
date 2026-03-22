sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/PollingManager",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (PollingManager, ModelStateRuntime, WorkflowContracts, ModelPathContracts) {
    "use strict";

    function isLockActive(oStateModel, StatePaths) {
        return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") === WorkflowContracts.EDIT_MODES.EDIT &&
            ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
    }

    function createLockPollingManager(mOptions, sLockMethod, sEventName, sErrorEventName, sTimerKey) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths;

        return new PollingManager({
            intervalMs: Number(mOptions.timerDefaults[sTimerKey]),
            checkFn: function () {
                if (!isLockActive(oStateModel, StatePaths)) {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock[sLockMethod] !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock." + sLockMethod });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock[sLockMethod]({ rootId: sRootId, sessionGuid: sSessionGuid }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            },
            eventName: sEventName,
            errorEventName: sErrorEventName
        });
    }

    function createHeartbeatManager(mOptions) {
        var oComponent = mOptions.component;

        oComponent._oHeartbeat = createLockPollingManager(mOptions, "heartbeat", "heartbeat", "heartbeatError", "heartbeatMs");
        return oComponent._oHeartbeat;
    }

    function createLockStatusManager(mOptions) {
        var oComponent = mOptions.component;

        oComponent._oLockStatus = createLockPollingManager(mOptions, "status", "status", "statusError", "lockStatusMs");
        return oComponent._oLockStatus;
    }

    function createSupportManagers(mOptions) {
        var oComponent = mOptions.component;
        oComponent._oGcd = new mOptions.managers.GCDManager({ intervalMs: Number(mOptions.timerDefaults.gcdMs) });
        oComponent._oActivity = new mOptions.managers.ActivityMonitor({ idleMs: Number(mOptions.timerDefaults.idleMs) });
        return {
            gcd: oComponent._oGcd,
            activity: oComponent._oActivity
        };
    }

    return {
        createHeartbeatManager: createHeartbeatManager,
        createLockStatusManager: createLockStatusManager,
        createSupportManagers: createSupportManagers
    };
});
