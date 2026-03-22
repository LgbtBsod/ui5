sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts"
], function (ModelStateRuntime, WorkflowContracts, ModelPathContracts) {
    "use strict";

    function createHeartbeatManager(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths;

        oComponent._oHeartbeat = new mOptions.managers.HeartbeatManager({
            intervalMs: Number(mOptions.timerDefaults.heartbeatMs),
            heartbeatFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") !== WorkflowContracts.EDIT_MODES.EDIT ||
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.heartbeat !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.heartbeat" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.heartbeat({ rootId: sRootId, sessionGuid: sSessionGuid }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            }
        });
        return oComponent._oHeartbeat;
    }

    function createLockStatusManager(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var StatePaths = mOptions.statePaths;

        oComponent._oLockStatus = new mOptions.managers.LockStatusMonitor({
            intervalMs: Number(mOptions.timerDefaults.lockStatusMs),
            checkFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") !== WorkflowContracts.EDIT_MODES.EDIT ||
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") !== WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.status !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.status" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.status({ rootId: sRootId, sessionGuid: sSessionGuid }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            }
        });
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
