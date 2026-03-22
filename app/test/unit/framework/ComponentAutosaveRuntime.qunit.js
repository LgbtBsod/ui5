sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentAutosaveRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (ComponentAutosaveRuntime, StatePaths) {
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

    QUnit.module("ComponentAutosaveRuntime");

    QUnit.test("autosave lock health requires valid owned lock without conflict", function (assert) {
        var oHealthyState = createStateModel({
            "/workflow/detail/editMode": "EDIT",
            "/workflow/detail/lock/state": "EDIT_LOCKED",
            "/isDirty": true,
            "/activeObjectId": "CHK-1",
            "/persistence/hasValidLock": true,
            "/persistence/lockOwnerSessionMatches": true,
            "/hasConflict": false
        });
        var oConflictState = createStateModel({
            "/workflow/detail/editMode": "EDIT",
            "/workflow/detail/lock/state": "EDIT_LOCKED",
            "/isDirty": true,
            "/activeObjectId": "CHK-1",
            "/persistence/hasValidLock": true,
            "/persistence/lockOwnerSessionMatches": true,
            "/hasConflict": true
        });
        var oInvalidLockState = createStateModel({
            "/workflow/detail/editMode": "EDIT",
            "/workflow/detail/lock/state": "EDIT_LOCKED",
            "/isDirty": true,
            "/activeObjectId": "CHK-1",
            "/persistence/hasValidLock": false,
            "/persistence/lockOwnerSessionMatches": true,
            "/hasConflict": false
        });

        assert.strictEqual(ComponentAutosaveRuntime.hasHealthyAutosaveLockState(oHealthyState, StatePaths), true, "healthy owned lock stays autosave-eligible");
        assert.strictEqual(ComponentAutosaveRuntime.hasHealthyAutosaveLockState(oConflictState, StatePaths), false, "probe conflict disables autosave eligibility");
        assert.strictEqual(ComponentAutosaveRuntime.hasHealthyAutosaveLockState(oInvalidLockState, StatePaths), false, "missing valid lock disables autosave eligibility");
    });
});
