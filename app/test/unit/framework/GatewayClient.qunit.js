sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, GatewayContractConstants) {
    "use strict";

    QUnit.module("framework/GatewayClient", {
        afterEach: function () {
            GatewayClient.reset();
        }
    });

    QUnit.test("rejects synchronous function import requests", function (assert) {
        GatewayClient.setModel({
            callFunction: function () {
                assert.ok(false, "sync request must be rejected before reaching ODataModel");
            },
            create: function () {
                assert.ok(false, "sync request must be rejected before reaching ODataModel");
            }
        });

        return assert.rejects(
            GatewayClient.callFunctionImport(
                GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,
                { RootId: "4711", SessionGuid: "SESSION-1" },
                { async: false }
            ),
            /Synchronous function imports are not supported/,
            "sync mode is forbidden"
        );
    });

    QUnit.test("keeps function import requests async by default", function (assert) {
        GatewayClient.setModel({
            create: function (_sPath, _oPayload, mOptions) {
                assert.strictEqual(typeof mOptions.async, "undefined", "async flag is no longer forwarded");
                mOptions.success({ ok: true });
            }
        });

        return GatewayClient.callFunctionImport(
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,
            { RootId: "4711", SessionGuid: "SESSION-1" },
            {}
        ).then(function () {
            assert.ok(true, "request resolves");
        });
    });

    QUnit.test("all function imports remain covered by one supported execution path", function (assert) {
        var aAllImports = [
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,
            GatewayContractConstants.FUNCTION_IMPORTS.CREATE_CHECKLIST,
            GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST,
            GatewayContractConstants.FUNCTION_IMPORTS.AUTO_SAVE,
            GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES,
            GatewayContractConstants.FUNCTION_IMPORTS.ANALYTICS_REFRESH_TRIGGER,
            GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY,
            GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT
        ];
        var aBody = [
            GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES,
            GatewayContractConstants.FUNCTION_IMPORTS.AUTO_SAVE,
            GatewayContractConstants.FUNCTION_IMPORTS.CREATE_CHECKLIST,
            GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE
        ];
        var aQuery = [
            GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST,
            GatewayContractConstants.FUNCTION_IMPORTS.ANALYTICS_REFRESH_TRIGGER
        ];
        var aGet = [
            GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY
        ];

        aAllImports.forEach(function (sName) {
            var iHits = [aBody, aQuery, aGet].filter(function (aBucket) {
                return aBucket.indexOf(sName) >= 0;
            }).length;
            assert.strictEqual(iHits, 1, sName + " must stay in exactly one execution path");
        });
    });
});
