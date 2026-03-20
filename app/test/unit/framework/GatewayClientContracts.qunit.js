/**
 * GatewayClientContracts.qunit.js
 * Regression guard for C-07 (SetChecklistStatus) and C-06 (Lock in BODY).
 * Place at: app/test/unit/framework/GatewayClientContracts.qunit.js
 */
sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientSupport"
], function (Contracts, Support) {
    "use strict";

    QUnit.module("service/backend/GatewayClientContracts");

    /* Every FunctionImport from metadata.xml must appear in exactly one list */
    var ALL_IMPORTS = [
        "LockAcquire", "LockHeartbeat", "LockRelease",
        "CreateChecklist", "CopyChecklist", "AutoSave", "SaveChanges",
        "SetChecklistStatus", "AnalyticsRefreshTrigger",
        "GetHierarchy", "ReportExport"
    ];
    var BODY  = Contracts.DIRECT_FUNCTION_BODY_ALLOWLIST;
    var QUERY = Contracts.DIRECT_FUNCTION_QUERY_ALLOWLIST;
    var GET   = Contracts.DIRECT_GET_FUNCTION_ALLOWLIST;

    QUnit.test("all FunctionImports covered by exactly one allowlist", function (assert) {
        ALL_IMPORTS.forEach(function (name) {
            var hits = [BODY, QUERY, GET].filter(function (list) {
                return Support.allowlisted(name, list);
            }).length;
            assert.strictEqual(hits, 1, name + " must appear in exactly one allowlist (found " + hits + ")");
        });
    });

    QUnit.test("C-07: SetChecklistStatus in BODY (was missing, caused runtime throw)", function (assert) {
        assert.ok(Support.allowlisted("SetChecklistStatus", BODY), "SetChecklistStatus in BODY");
    });

    QUnit.test("C-06: Lock operations in BODY (SessionGuid must not appear in URL)", function (assert) {
        ["LockAcquire", "LockHeartbeat", "LockRelease"].forEach(function (name) {
            assert.ok(Support.allowlisted(name, BODY),  name + " in BODY");
            assert.notOk(Support.allowlisted(name, QUERY), name + " NOT in QUERY");
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
