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

    QUnit.test("unload release queues keepalive fetch when token and payload are available", function (assert) {
        var fnOriginalFetch = window.fetch;
        var aCalls = [];

        window.fetch = function (sUrl, mOptions) {
            aCalls.push({ url: sUrl, options: mOptions });
            return Promise.resolve();
        };

        assert.strictEqual(
            ComponentLockReleaseRuntime.tryBeaconLockRelease("http://example.test/LockRelease", { RootId: "1", SessionGuid: "S1" }, "token"),
            true,
            "Keepalive release was queued"
        );
        assert.strictEqual(aCalls.length, 1, "Fetch was called once");
        assert.ok(aCalls[0].url.indexOf("RootId=1") >= 0, "RootId is sent as a function import parameter");
        assert.ok(aCalls[0].url.indexOf("SessionGuid=S1") >= 0, "SessionGuid is sent as a function import parameter");
        assert.strictEqual(aCalls[0].options.method, "POST", "Release uses POST");
        assert.strictEqual(aCalls[0].options.keepalive, true, "Release uses keepalive transport");
        assert.strictEqual(aCalls[0].options.headers["X-CSRF-Token"], "token", "CSRF token is forwarded");

        window.fetch = fnOriginalFetch;
    });

    QUnit.test("unload release returns false when fetch transport is unavailable", function (assert) {
        var fnOriginalFetch = window.fetch;

        window.fetch = undefined;
        assert.strictEqual(
            ComponentLockReleaseRuntime.tryBeaconLockRelease("http://example.test/LockRelease", { RootId: "1", SessionGuid: "S1" }, "token"),
            false,
            "Keepalive release falls back when fetch is unavailable"
        );
        window.fetch = fnOriginalFetch;
    });
});
