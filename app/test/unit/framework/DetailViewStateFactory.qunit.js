sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailViewStateFactory"
], function (DetailViewStateFactory) {
    "use strict";

    function createControllerDouble() {
        return {
            getOwnerComponent: function () {
                return {
                    getModel: function () {
                        return {
                            getResourceBundle: function () {
                                return {
                                    getText: function (sKey) {
                                        return sKey;
                                    }
                                };
                            }
                        };
                    }
                };
            }
        };
    }

    QUnit.module("framework/DetailViewStateFactory");

    QUnit.test("wires table specs into detail sections", function (assert) {
        var oState = DetailViewStateFactory.create(createControllerDouble());

        assert.strictEqual(oState.detailSections.checks.kind, "check", "checks section kind is defined");
        assert.strictEqual(oState.detailSections.checks.tableSpec.kind, "check", "checks table spec is wired");
        assert.strictEqual(oState.detailSections.checks.tableSpec.rowsPath, "/checks", "checks table path is wired");
        assert.strictEqual(oState.detailSections.barriers.kind, "barrier", "barriers section kind is defined");
        assert.strictEqual(oState.detailSections.barriers.tableSpec.kind, "barrier", "barriers table spec is wired");
        assert.strictEqual(oState.detailSections.barriers.tableSpec.rowsPath, "/barriers", "barriers table path is wired");
    });

    QUnit.test("creates expanded dialog specs from the same entity config", function (assert) {
        var oState = DetailViewStateFactory.create(createControllerDouble());

        assert.strictEqual(oState.detailExpandedDialogs.checks.tableSpec.expanded, true, "checks expanded spec is marked as expanded");
        assert.strictEqual(oState.detailExpandedDialogs.checks.tableSpec.ignoreNarrowViewport, true, "checks expanded spec ignores narrow viewport fallback");
        assert.strictEqual(oState.detailExpandedDialogs.barriers.tableSpec.expanded, true, "barriers expanded spec is marked as expanded");
        assert.strictEqual(oState.detailExpandedDialogs.barriers.tableSpec.ignoreNarrowViewport, true, "barriers expanded spec ignores narrow viewport fallback");
        assert.strictEqual(oState.detailExpandedDialogs.barriers.tableSpec.desktopVisibleRowCount, 8, "expanded dialogs keep fixed desktop row count");
    });
});
