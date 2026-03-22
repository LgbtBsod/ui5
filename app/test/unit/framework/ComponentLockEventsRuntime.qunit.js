sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockEventsRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (ComponentLockEventsRuntime, StatePaths, ModelContracts) {
    "use strict";

    function createStateModel(mSeed) {
        var mState = Object.assign({}, mSeed || {});
        return {
            getProperty: function (sPath) {
                return mState[sPath];
            },
            setProperty: function (sPath, vValue) {
                mState[sPath] = vValue;
            }
        };
    }

    QUnit.module("ComponentLockEventsRuntime");

    QUnit.test("probe lock invalidation revokes autosave lock eligibility", function (assert) {
        var oStateModel = createStateModel({
            "/hasConflict": false,
            "/persistence/hasValidLock": true,
            "/persistence/lockOwnerSessionMatches": true
        });
        var oShellModel = createStateModel({});

        ComponentLockEventsRuntime.invalidateProbeLockHealth(oStateModel, oShellModel, StatePaths, ModelContracts.MODEL_PATHS);

        assert.strictEqual(oStateModel.getProperty("/hasConflict"), true, "conflict flag is raised");
        assert.strictEqual(oStateModel.getProperty(StatePaths.PERSISTENCE_HAS_VALID_LOCK), false, "valid lock flag is revoked");
        assert.strictEqual(oStateModel.getProperty(StatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES), false, "session ownership flag is revoked");
        assert.strictEqual(oShellModel.getProperty(ModelContracts.MODEL_PATHS.SHELL_LOCK).ok, false, "shell lock state becomes unhealthy");
    });
});
