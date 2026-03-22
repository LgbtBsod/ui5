sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/controller/search/internal/SearchFilterLifecycleBehavior",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (JSONModel, SearchFilterLifecycleBehavior, ModelConstants, StatePaths) {
    "use strict";

    QUnit.module("framework/SearchFilterLifecycleBehavior");

    QUnit.test("clear handler resets custom segments via SmartFilterBar event only", function (assert) {
        var done = assert.async();
        var fnClear;
        var oStateModel = new JSONModel({
            search: {},
            searchMode: "EXACT"
        });
        oStateModel.setProperty(StatePaths.SEARCH_CHECKS_FAIL_SEGMENT, "FAIL");
        oStateModel.setProperty(StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT, "FAIL");

        var oController = {
            _bSearchClearHandlerBound: false,
            getModel: function (sName) {
                return sName === ModelConstants.MODELS.STATE ? oStateModel : null;
            },
            byId: function (sId) {
                if (sId === "searchSmartFilterBar") {
                    return {
                        attachClear: function (fnHandler) {
                            fnClear = fnHandler;
                        }
                    };
                }
                return {
                    setSelectedKey: function () {}
                };
            }
        };

        SearchFilterLifecycleBehavior.onSmartFilterInitialise(oController, function () {});
        assert.strictEqual(typeof fnClear, "function", "SmartFilterBar clear handler is bound");

        fnClear();
        setTimeout(function () {
            assert.strictEqual(oStateModel.getProperty(StatePaths.SEARCH_CHECKS_FAIL_SEGMENT), "ALL", "checks segment reset");
            assert.strictEqual(oStateModel.getProperty(StatePaths.SEARCH_BARRIERS_FAIL_SEGMENT), "ALL", "barriers segment reset");
            done();
        }, 0);
    });
});
