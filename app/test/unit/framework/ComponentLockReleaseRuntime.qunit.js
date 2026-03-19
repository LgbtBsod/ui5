sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLockReleaseRuntime"
], function (ComponentLockReleaseRuntime) {
    "use strict";

    QUnit.module("ComponentLockReleaseRuntime");

    QUnit.test("buildLockReleaseUrl normalizes trailing slash", function (assert) {
        var oStateModel = {
            getProperty: function (sPath) {
                if (sPath === "/backendServiceUrl") {
                    return "http://example.test/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/";
                }
                return "";
            }
        };

        assert.strictEqual(
            ComponentLockReleaseRuntime.buildLockReleaseUrl(oStateModel),
            "http://example.test/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/LockRelease",
            "Lock release URL is normalized"
        );
    });

    QUnit.test("readActiveLockPayload returns lock release input only for owned edit locks", function (assert) {
        var oStateModel = {
            getProperty: function (sPath) {
                if (sPath === "/activeObjectId") {
                    return "4711";
                }
                if (sPath === "/sessionId") {
                    return "SESSION-1";
                }
                if (sPath === "/workflow/detail/editMode") {
                    return "EDIT";
                }
                if (sPath === "/workflow/detail/lock/state") {
                    return "EDIT_LOCKED";
                }
                return "";
            }
        };

        assert.deepEqual(ComponentLockReleaseRuntime.readActiveLockPayload(oStateModel), {
            RootId: "4711",
            SessionGuid: "SESSION-1"
        }, "Owned edit lock payload is exposed");
    });

    QUnit.test("readActiveLockPayload returns null outside owned edit lock state", function (assert) {
        var oStateModel = {
            getProperty: function (sPath) {
                if (sPath === "/activeObjectId") {
                    return "4711";
                }
                if (sPath === "/sessionId") {
                    return "SESSION-1";
                }
                if (sPath === "/workflow/detail/editMode") {
                    return "READ";
                }
                if (sPath === "/workflow/detail/lock/state") {
                    return "READ_ONLY";
                }
                return "";
            }
        };

        assert.strictEqual(ComponentLockReleaseRuntime.readActiveLockPayload(oStateModel), null, "No payload is exposed for read-only state");
    });
});
