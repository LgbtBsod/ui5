sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClientRequestRuntime, GatewayContractConstants) {
    "use strict";

    QUnit.module("framework/GatewayClientRequestRuntime");

    QUnit.test("passes async flag to function import requests", function (assert) {
        var done = assert.async();
        var oModel = {
            callFunction: function (_sPath, mOptions) {
                assert.strictEqual(mOptions.async, false, "sync mode is forwarded to ODataModel.callFunction");
                mOptions.success({ ok: true });
            }
        };

        GatewayClientRequestRuntime.withDirectFunctionImportRequest(
            oModel,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,
            { RootId: "4711", SessionGuid: "SESSION-1" },
            {},
            { async: false }
        ).promise.then(function () {
            assert.ok(true, "request handle resolves");
            done();
        });
    });
});
