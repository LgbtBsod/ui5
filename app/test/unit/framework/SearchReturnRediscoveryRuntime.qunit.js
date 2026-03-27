sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchReturnRediscoveryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths"
], function (JSONModel, SearchReturnRediscoveryRuntime, StatePaths) {
    "use strict";

    function createController(mStateData, mViewData) {
        var mModels = {
            state: new JSONModel(mStateData || {}),
            view: new JSONModel(mViewData || {})
        };
        return {
            getModel: function (sName) {
                return mModels[sName];
            }
        };
    }

    function createItem(oObject) {
        return {
            getBindingContext: function () {
                return {
                    getObject: function () {
                        return oObject;
                    }
                };
            },
            getDomRef: function () {
                return {
                    scrollIntoView: function () {}
                };
            },
            setSelected: function (bValue) {
                this._selected = !!bValue;
            }
        };
    }

    function createTable(aItems) {
        return {
            _cleared: false,
            _selectedItem: null,
            getItems: function () {
                return aItems || [];
            },
            removeSelections: function () {
                this._cleared = true;
            },
            setSelectedItem: function (oItem) {
                this._selectedItem = oItem;
            }
        };
    }

    QUnit.module("SearchReturnRediscoveryRuntime");

    QUnit.test("prefers technical root id and clears context after successful rediscovery", function (assert) {
        var oController = createController({
            searchReturnContext: {
                rootId: "ROOT-2",
                checklistId: "CHK-00002",
                reason: "detailSaveCompleted",
                mode: "save",
                focusRequested: false,
                selectionRequested: true
            }
        }, {});
        var oMatchedItem = createItem({ Key: "ROOT-2", Id: "CHK-OTHER" });
        var oTable = createTable([
            createItem({ Key: "ROOT-1", Id: "CHK-00001" }),
            oMatchedItem
        ]);

        assert.strictEqual(SearchReturnRediscoveryRuntime.applyAfterSearchSuccess(oController, oTable), true, "return context is processed");
        assert.strictEqual(oController.getModel("view").getProperty("/selectedRowId"), "ROOT-2", "technical id is restored");
        assert.strictEqual(oController.getModel("view").getProperty("/selectedRowDisplayId"), "CHK-OTHER", "display id comes from matched row");
        assert.strictEqual(oController.getModel("view").getProperty("/hasSelection"), true, "selection state is restored");
        assert.strictEqual(oController.getModel("state").getProperty(StatePaths.SEARCH_RETURN_CONTEXT), null, "return context is cleared");
    });

    QUnit.test("falls back to checklist display id when root id is absent in visible rows", function (assert) {
        var oController = createController({
            searchReturnContext: {
                rootId: "ROOT-MISSING",
                checklistId: "CHK-00077",
                reason: "detailSaveCompleted",
                mode: "create",
                focusRequested: false,
                selectionRequested: true
            }
        }, {});
        var oTable = createTable([
            createItem({ Key: "ROOT-88", Id: "CHK-00077" })
        ]);

        assert.strictEqual(SearchReturnRediscoveryRuntime.applyAfterSearchSuccess(oController, oTable), true, "return context is processed");
        assert.strictEqual(oController.getModel("view").getProperty("/selectedRowId"), "ROOT-88", "matched technical id is taken from visible row");
        assert.strictEqual(oController.getModel("view").getProperty("/selectedRowDisplayId"), "CHK-00077", "display fallback works");
    });

    QUnit.test("delete return clears stale selection state", function (assert) {
        var oController = createController({
            searchReturnContext: {
                rootId: "ROOT-DELETE",
                reason: "detailDeleteCompleted",
                mode: SearchReturnRediscoveryRuntime.MODES.DELETE,
                focusRequested: false,
                selectionRequested: false
            }
        }, {
            selectedRowId: "ROOT-DELETE",
            selectedRowDisplayId: "CHK-DELETE",
            selectedRowIds: ["ROOT-DELETE"],
            selectionCount: 1,
            hasSelection: true,
            canCopy: true,
            canDelete: true
        });
        var oTable = createTable([]);

        assert.strictEqual(SearchReturnRediscoveryRuntime.applyAfterSearchSuccess(oController, oTable), true, "delete context is processed");
        assert.strictEqual(oController.getModel("view").getProperty("/selectedRowId"), "", "selected row id is reset");
        assert.strictEqual(oController.getModel("view").getProperty("/hasSelection"), false, "selection flag is reset");
        assert.strictEqual(oTable._cleared, true, "table selection is cleared");
    });
});
