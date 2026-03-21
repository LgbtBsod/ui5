sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAppRuntime"
], function (JSONModel, ComponentAppRuntime) {
    "use strict";

    QUnit.module("framework/ComponentAppRuntime");

    QUnit.test("releaseActiveLockOnLeave marks local leave state without backend call", function (assert) {
        var oStateModel = new JSONModel({
            activeObjectId: "CHK-00001",
            sessionId: "SESSION-1",
            workflow: {
                detail: {
                    editMode: "EDIT",
                    lock: {
                        state: "EDIT_LOCKED"
                    }
                }
            }
        });
        var oComponent = {
            _bLeaveReleasePending: false,
            _bLeaveReleaseAttempted: false,
            _bLeaveReleaseSent: false
        };

        oStateModel.setProperty("/workflow/detail/lock/state", "EDIT_LOCKED");
        assert.strictEqual(ComponentAppRuntime.releaseActiveLockOnLeave(oComponent, oStateModel, null), false, "leave release stays local when no backend model is available");
        assert.strictEqual(oComponent._bLeaveReleasePending, true, "pending marker is set");
        assert.strictEqual(oComponent._bLeaveReleaseAttempted, true, "attempt marker is set");
        assert.strictEqual(oComponent._bLeaveReleaseSent, false, "no backend success is assumed");

        ComponentAppRuntime.resetLeaveReleaseState(oComponent);
        assert.strictEqual(oComponent._bLeaveReleasePending, false, "pending marker resets");
        assert.strictEqual(oComponent._bLeaveReleaseAttempted, false, "attempt marker resets");
    });
});
