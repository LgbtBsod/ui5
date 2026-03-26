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
        assert.strictEqual(oBasic.type, "Edm.Binary", "basic info filter keeps binary type");
        assert.strictEqual(oCheck.property, "PARENT_KEY", "checks use canonical parent key");
        assert.strictEqual(oBarrier.property, "PARENT_KEY", "barriers use canonical parent key");
    });

    QUnit.test("buildDetailFilter formats according to the declared entity contract", function (assert) {
        var sRootId = "00112233445566778899AABBCCDDEEFF";
        var oBasic = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BASIC_INFO;
        var oCheck = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_CHECK;

        assert.strictEqual(
            ODataChecklistReadRuntime.buildDetailFilter(oBasic, sRootId),
            "DB_KEY eq binary'00112233445566778899AABBCCDDEEFF'",
            "basic info uses binary literal"
        );
        assert.strictEqual(
            ODataChecklistReadRuntime.buildDetailFilter(oCheck, sRootId),
            "PARENT_KEY eq binary'00112233445566778899AABBCCDDEEFF'",
            "check rows use canonical binary parent key"
        );
    });
});
