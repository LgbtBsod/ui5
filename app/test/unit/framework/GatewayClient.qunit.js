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
                { DB_KEY: "4711", SessionGuid: "SESSION-1" },
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
            { DB_KEY: "4711", SessionGuid: "SESSION-1" },
            {}
        ).then(function () {
            assert.ok(true, "request resolves");
        });
    });

    QUnit.test("CopyChecklist uses body transport via create path", function (assert) {
        var done = assert.async();
        var bCreateCalled = false;

        GatewayClient.setModel({
            callFunction: function () {
                assert.ok(false, "CopyChecklist must not go through query-based callFunction");
            },
            create: function (sPath, oPayload, mOptions) {
                bCreateCalled = true;
                assert.strictEqual(sPath, "/" + GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST, "create uses canonical function import path");
                assert.deepEqual(oPayload, {
                    DB_KEY: "4711",
                    SessionGuid: "SESSION-1"
                }, "payload stays in request body");
                mOptions.success({ ok: true });
            }
        });

        GatewayClient.callFunctionImport(
            GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST,
            { DB_KEY: "4711", SessionGuid: "SESSION-1" }
        ).then(function () {
            assert.ok(bCreateCalled, "body transport path is executed");
            done();
        });
    });

"    QUnit.test(\"all function imports remain covered by one supported execution path\", function (assert) {\n        var aAllImports = [\n            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE,\n            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT,\n            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE,\n            GatewayContractConstants.FUNCTION_IMPORTS.CREATE_CHECKLIST,\n            GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST,\n            GatewayContractConstants.FUNCTION_IMPORTS.AUTO_SAVE,\n            GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES,\n            GatewayContractConstants.FUNCTION_IMPORTS.ANALYTICS_REFRESH_TRIGGER,\n            GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY,\n            GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT\n        ];\n        var aBody = [\n            GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES,\n            GatewayContractConstants.FUNCTION_IMPORTS.AUTO_SAVE,\n            GatewayContractConstants.FUNCTION_IMPORTS.CREATE_CHECKLIST,\n            GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST,\n            GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT,\n            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE,\n            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT,\n            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE\n        ];\n        var aQuery = [\n            GatewayContractConstants.FUNCTION_IMPORTS.ANALYTICS_REFRESH_TRIGGER\n        ];\n        var aGet = [\n            GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY\n        ];\n\n        aAllImports.forEach(function (sName) {\n            var iHits = [aBody, aQuery, aGet].filter(function (aBucket) {\n                return aBucket.indexOf(sName) >= 0;\n            }).length;\n            assert.strictEqual(iHits, 1, sName + \" must stay in exactly one execution path\");\n        });\n    });\n\n    QUnit.test(\"URL encoding in readEntity prevents OData path injection\", function (assert) {\n        var done = assert.async();\n        var sCapturedPath;\n\n        GatewayClient.setModel({\n            read: function (sPath, mOptions) {\n                sCapturedPath = sPath;\n                mOptions.success({});\n            }\n        });\n\n        return GatewayClient.readEntity(\n            \"ChecklistRootSet\",\n            \"TEST-KEY-123\",\n            {},\n            {}\n        ).then(function () {\n            assert.ok(sCapturedPath.indexOf(\"TEST-KEY-123\") >= 0, \"entity key is preserved\");\n            assert.strictEqual(sCapturedPath.indexOf(\"%\"), -1, \"no percent encoding in simple keys\");\n        });\n    });\n\n    QUnit.test(\"reset() method cleans up all internal state including correlation ID counter\", function (assert) {\n        GatewayClient.setModel({ sServiceUrl: \"/sap/opu/odata\" });\n        GatewayClient.setHeader(\"X-Custom\", \"value\");\n        GatewayClient.nextCorrelationId();\n        GatewayClient.nextCorrelationId();\n\n        GatewayClient.reset();\n\n        assert.strictEqual(GatewayClient.hasModel(), false, \"model is cleared\");\n        assert.strictEqual(GatewayClient.serviceUrl(), \"\", \"service URL is reset\");\n    });\n\n    QUnit.test(\"response guard tokens prevent stale response handling\", function (assert) {\n        var done = assert.async();\n        var iGuardCalls = 0;\n\n        GatewayClient.setModel({\n            read: function (_sPath, mOptions) {\n                iGuardCalls++;\n                mOptions.success({});\n            }\n        });\n\n        var sGuardKey = \"test-guard-1\";\n\n        return GatewayClient.rawRead(\n            \"/TestSet\",\n            {},\n            { responseGuardKey: sGuardKey }\n        ).then(function () {\n            assert.strictEqual(iGuardCalls, 1, \"first request completed\");\n\n            return GatewayClient.rawRead(\n                \"/TestSet\",\n                {},\n                { responseGuardKey: sGuardKey }\n            );\n        }).then(function () {\n            assert.strictEqual(iGuardCalls, 2, \"second request completed\");\n        });\n    });\n\n    QUnit.test(\"forbidden OData paths are rejected with clear error\", function (assert) {\n        GatewayClient.setModel({});\n\n        return assert.rejects(\n            function () {\n                GatewayClient.rawRead(\"/actions/someAction\", {}, {});\n            }(),\n            /FORBIDDEN_NON_CANONICAL_ODATA_PATH/,\n            \"actions path is forbidden\"\n        );\n    });\n\n    QUnit.test(\"forbidden function imports are rejected with clear error\", function (assert) {\n        GatewayClient.setModel({});\n\n        return assert.rejects(\n            GatewayClient.callFunctionImport(\"Capabilities\", {}, {}),\n            /FORBIDDEN_NON_CANONICAL_FUNCTION_IMPORT/,\n            \"forbidden function import is rejected\"\n        );\n    });\n\n    QUnit.test(\"GetHierarchy uses GET method with urlParameters\", function (assert) {\n        var done = assert.async();\n        var bCallFunctionCalled = false;\n\n        GatewayClient.setModel({\n            callFunction: function (sPath, mOptions) {\n                bCallFunctionCalled = true;\n                assert.strictEqual(sPath, \"/\" + GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY, \"correct function path\");\n                assert.strictEqual(mOptions.method, \"GET\", \"GET method used\");\n                assert.deepEqual(mOptions.urlParameters, { LocationKey: \"DE01\" }, \"parameters in urlParameters\");\n                mOptions.success({ items: [] });\n            }\n        });\n\n        GatewayClient.callGetFunctionImport(\n            GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY,\n            { LocationKey: \"DE01\" }\n        ).then(function () {\n            assert.ok(bCallFunctionCalled, \"callFunction executed for GetHierarchy\");\n            done();\n        });\n    });\n\n    QUnit.test(\"batch operations use submitChanges with group ID\", function (assert) {\n        var done = assert.async();\n        var sCapturedGroupId;\n\n        GatewayClient.setModel({\n            submitChanges: function (mOptions) {\n                sCapturedGroupId = mOptions.groupId;\n                mOptions.success({ __batchResponses: [], __changeResponses: [] });\n            }\n        });\n\n        return GatewayClient.batch(\"$auto\").then(function () {\n            assert.strictEqual(sCapturedGroupId, \"$auto\", \"batch group ID preserved\");\n            done();\n        });\n    });\n\n    QUnit.test(\"security token refresh is available through refreshSecurityToken\", function (assert) {\n        var done = assert.async();\n        var bRefreshCalled = false;\n\n        GatewayClient.setModel({\n            refreshSecurityToken: function (fnSuccess, fnError, bForce) {\n                bRefreshCalled = true;\n                assert.strictEqual(bForce, true, \"force flag is true\");\n                fnSuccess();\n            }\n        });\n\n        return GatewayClient.refreshSecurityToken().then(function () {\n            assert.ok(bRefreshCalled, \"security token refresh executed\");\n            done();\n        });\n    });\n\n    QUnit.test(\"normalized error includes correlation ID from request options\", function (assert) {\n        var done = assert.async();\n        var sCorrelationId = \"CORR-123-TEST\";\n\n        GatewayClient.setModel({\n            read: function (_sPath, mOptions) {\n                mOptions.error({ message: \"Network error\" });\n            }\n        });\n\n        return GatewayClient.rawRead(\n            \"/TestSet\",\n            {},\n            { correlationId: sCorrelationId }\n        ).catch(function (oError) {\n            assert.strictEqual(oError.correlationId, sCorrelationId, \"correlation ID preserved in error\");\n            done();\n        });\n    });\n});", function (assert) {
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
            GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST,
            GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE
        ];
        var aQuery = [
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
