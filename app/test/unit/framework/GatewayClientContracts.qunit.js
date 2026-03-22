/**
 * GatewayClientContracts.qunit.js
 * Regression guard for C-06 (Lock in BODY) and forbidden path filters.
 * Place at: app/test/unit/framework/GatewayClientContracts.qunit.js
 */
sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (Contracts, GatewayContractConstants) {
    "use strict";

    QUnit.module("service/backend/GatewayClientContracts");

    /* Every FunctionImport from metadata.xml must appear in exactly one list */
    var ALL_IMPORTS = [
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
    var BODY  = Contracts.DIRECT_FUNCTION_BODY_ALLOWLIST;
    var QUERY = Contracts.DIRECT_FUNCTION_QUERY_ALLOWLIST;
    var GET   = Contracts.DIRECT_GET_FUNCTION_ALLOWLIST;

    QUnit.test("all FunctionImports covered by exactly one allowlist", function (assert) {
        ALL_IMPORTS.forEach(function (name) {
            var hits = [BODY, QUERY, GET].filter(function (list) {
                return Contracts.allowlisted(name, list);
            }).length;
            assert.strictEqual(hits, 1, name + " must appear in exactly one allowlist (found " + hits + ")");
        });
    });

    QUnit.test("C-06: Lock operations in BODY (SessionGuid must not appear in URL)", function (assert) {
        [
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT,
            GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE
        ].forEach(function (name) {
            assert.ok(Contracts.allowlisted(name, BODY),  name + " in BODY");
            assert.notOk(Contracts.allowlisted(name, QUERY), name + " NOT in QUERY");
        });
    });

    QUnit.test("FORBIDDEN_PATH_PATTERNS block dangerous paths", function (assert) {
        ["/FrontendRuntimeSettings", "/capabilities", "/ChecklistRoots", "/SearchRows",
         "/ChecklistChecksSet", "/ChecklistBarriersSet"].forEach(function (p) {
            var blocked = Contracts.FORBIDDEN_PATH_PATTERNS.some(function (rx) { return rx.test(p); });
            assert.ok(blocked, p + " is blocked");
        });
    });
});
