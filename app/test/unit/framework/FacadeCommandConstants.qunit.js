sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/FacadeCommandConstants"
], function (FacadeCommandConstants) {
    "use strict";

    QUnit.module("FacadeCommandConstants");

    QUnit.test("exports canonical detail and search command ids", function (assert) {
        assert.strictEqual(FacadeCommandConstants.DETAIL.AUTOSAVE, "autosave", "detail commands are canonical");
        assert.strictEqual(FacadeCommandConstants.SEARCH.EXECUTE_SEARCH, "executeSearch", "search commands are canonical");
    });
});
