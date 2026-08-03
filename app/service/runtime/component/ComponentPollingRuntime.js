sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/PollingManager",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/RuntimeEventContracts"
], function (PollingManager, ModelStateRuntime, WorkflowContracts, StatePaths, RuntimeEventContracts) {
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
                var sDbKey = ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock[sLockMethod]({ dbKey: sDbKey, sessionGuid: sSessionGuid }).then(function (oRes) {
                    return Object.assign({ dbKey: sDbKey, sessionGuid: sSessionGuid }, oRes || {});
                });
            },
            eventName: sEventName,
            errorEventName: sErrorEventName
        });
    }

    function createHeartbeatManager(mOptions) {
        var oComponent = mOptions.component;

        oComponent._oHeartbeat = createLockPollingManager(mOptions, "heartbeat", RuntimeEventContracts.HEARTBEAT, RuntimeEventContracts.HEARTBEAT_ERROR, "heartbeatMs");
        return oComponent._oHeartbeat;
    }

    function createLockStatusManager(mOptions) {
        var oComponent = mOptions.component;

        oComponent._oLockStatus = createLockPollingManager(mOptions, "status", RuntimeEventContracts.STATUS, RuntimeEventContracts.STATUS_ERROR, "lockStatusMs");
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
