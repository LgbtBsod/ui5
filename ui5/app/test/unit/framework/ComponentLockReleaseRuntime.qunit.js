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

    QUnit.test("unload beacon release stays disabled for productive Gateway compatibility", function (assert) {
        assert.strictEqual(
            ComponentLockReleaseRuntime.tryBeaconLockRelease("http://example.test/LockRelease", { RootId: "1" }, "token"),
            false,
            "Beacon release is disabled"
        );
    });
});
