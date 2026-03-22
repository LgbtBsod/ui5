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
            "/workflow/detail/lock/state": "EDIT_LOCKED",
            "/persistence/hasValidLock": true,
            "/persistence/lockOwnerSessionMatches": true,
            "/hasConflict": false
        });
        var oConflictState = createStateModel({
            "/workflow/detail/lock/state": "EDIT_LOCKED",
            "/persistence/hasValidLock": true,
            "/persistence/lockOwnerSessionMatches": true,
            "/hasConflict": true
        });

        assert.strictEqual(ComponentAutosaveRuntime.hasHealthyAutosaveLockState(oHealthyState, StatePaths), true, "healthy owned lock stays autosave-eligible");
        assert.strictEqual(ComponentAutosaveRuntime.hasHealthyAutosaveLockState(oConflictState, StatePaths), false, "probe conflict disables autosave eligibility");
    });
});
