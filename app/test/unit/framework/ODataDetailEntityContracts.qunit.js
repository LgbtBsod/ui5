sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistReadRuntime"
], function (ODataDetailEntityContracts, ODataChecklistReadRuntime) {
    "use strict";

    QUnit.module("framework/ODataDetailEntityContracts");

    QUnit.test("keeps per-entity root filter contract explicit", function (assert) {
        var oBasic = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BASIC_INFO;
        var oCheck = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_CHECK;
        var oBarrier = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BARRIER;

        assert.strictEqual(oBasic.property, "DB_KEY", "basic info uses canonical DB_KEY");
        assert.strictEqual(oBasic.type, "Edm.String", "basic info filter uses backend's bin-to-hex string type");
        assert.strictEqual(oCheck.property, "PARENT_KEY", "checks use canonical parent key");
        assert.strictEqual(oBarrier.property, "PARENT_KEY", "barriers use canonical parent key");
    });

    QUnit.test("buildDetailFilter formats according to the declared entity contract", function (assert) {
        var sRootId = "00112233445566778899AABBCCDDEEFF";
        var oBasic = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BASIC_INFO;
        var oCheck = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_CHECK;

        assert.strictEqual(
            ODataChecklistReadRuntime.buildDetailFilter(oBasic, sRootId),
            "DB_KEY eq '00112233445566778899AABBCCDDEEFF'",
            "basic info uses string literal matching backend's hex32 representation"
        );
        assert.strictEqual(
            ODataChecklistReadRuntime.buildDetailFilter(oCheck, sRootId),
            "PARENT_KEY eq '00112233445566778899AABBCCDDEEFF'",
            "check rows use canonical string parent key"
        );
    });

    QUnit.test("select contracts include productive root and integration fallback fields", function (assert) {
        assert.ok(
            ODataDetailEntityContracts.SELECTS.CHECKLIST_ROOT.indexOf("IntegrationFlag") >= 0,
            "root select requests IntegrationFlag"
        );
        assert.ok(
            ODataDetailEntityContracts.SELECTS.CHECKLIST_BASIC_INFO.indexOf("ObserverName") >= 0,
            "basic info select requests ObserverName"
        );
        assert.ok(
            ODataDetailEntityContracts.SELECTS.CHECKLIST_BASIC_INFO.indexOf("ObservedName") >= 0,
            "basic info select requests ObservedName"
        );
    });
});
