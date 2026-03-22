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

        assert.strictEqual(oBasic.property, "RootKey", "basic info stays on binary RootKey");
        assert.strictEqual(oBasic.type, "Edm.Binary", "basic info filter keeps binary type");
        assert.strictEqual(oCheck.property, "RootId", "checks keep RootId filter alias");
        assert.strictEqual(oBarrier.property, "RootId", "barriers keep RootId filter alias");
    });

    QUnit.test("buildDetailFilter formats according to the declared entity contract", function (assert) {
        var sRootId = "00112233445566778899AABBCCDDEEFF";
        var oBasic = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BASIC_INFO;
        var oCheck = ODataDetailEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_CHECK;

        assert.strictEqual(
            ODataChecklistReadRuntime.buildDetailFilter(oBasic, sRootId),
            "RootKey eq binary'00112233445566778899AABBCCDDEEFF'",
            "basic info uses binary literal"
        );
        assert.strictEqual(
            ODataChecklistReadRuntime.buildDetailFilter(oCheck, sRootId),
            "RootId eq '00112233445566778899AABBCCDDEEFF'",
            "check rows keep string alias literal"
        );
    });
});
