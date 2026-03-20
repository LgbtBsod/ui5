sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FacadeCommandContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FacadeCommandConstants"
], function (FacadeCommandContracts, FacadeCommandConstants) {
    "use strict";

    QUnit.module("FacadeCommandContracts");

    QUnit.test("exports canonical detail and search command ids", function (assert) {
        assert.strictEqual(FacadeCommandContracts.DETAIL_METHODS.AUTOSAVE, FacadeCommandConstants.DETAIL.AUTOSAVE, "detail commands are canonical");
        assert.strictEqual(FacadeCommandContracts.SEARCH_METHODS.EXECUTE_SEARCH, FacadeCommandConstants.SEARCH.EXECUTE_SEARCH, "search commands are canonical");
    });
});
