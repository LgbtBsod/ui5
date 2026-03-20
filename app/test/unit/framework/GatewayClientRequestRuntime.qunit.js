sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClientRequestRuntime, GatewayContractConstants) {
    "use strict";

    QUnit.module("framework/GatewayClientRequestRuntime");

    QUnit.test("rejects synchronous function import requests", function (assert) {
        var oModel = {
            callFunction: function (_sPath, mOptions) {
                assert.ok(false, "sync request must be rejected before reaching ODataModel");
            }
        };

        assert.throws(function () {
            GatewayClientRequestRuntime.withDirectFunctionImportRequest(
                oModel,
                GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,
                { RootId: "4711", SessionGuid: "SESSION-1" },
                {},
                { async: false }
            );
        }, /Synchronous function imports are not supported/, "sync mode is forbidden");
    });

    QUnit.test("keeps function import requests async by default", function (assert) {
        var done = assert.async();
        var oModel = {
            create: function (_sPath, _oPayload, mOptions) {
                assert.strictEqual(typeof mOptions.async, "undefined", "async flag is no longer forwarded");
                mOptions.success({ ok: true });
            }
        };

        GatewayClientRequestRuntime.withDirectFunctionImportRequest(
            oModel,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,
            { RootId: "4711", SessionGuid: "SESSION-1" },
            {},
            {}
        ).promise.then(function () {
            assert.ok(true, "request handle resolves");
            done();
        });
    });
});
