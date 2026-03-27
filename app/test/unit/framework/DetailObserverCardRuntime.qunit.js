sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controller/detail/DetailObserverCardRuntime"
], function (DetailObserverCardRuntime) {
    "use strict";

    function createControllerStub() {
        return {
            onPersonSuggest: function () {},
            onPersonInputChange: function () {},
            onPersonSuggestionSelected: function () {},
            formatI18nByKey: function (sKey) { return sKey; }
        };
    }

    function createHooksStub() {
        return {
            wrapEditableField: function (_oController, oControl) {
                return oControl;
            }
        };
    }

    function readFormatter(sTarget) {
        var oContent = DetailObserverCardRuntime.createContent(createControllerStub(), sTarget, createHooksStub());
        var oReadText = oContent.getItems()[0];
        return oReadText.getBindingInfo("text").formatter;
    }

    QUnit.module("framework/DetailObserverCardRuntime");

    QUnit.test("observer read-only text falls back to integration name when pernr is empty", function (assert) {
        var fnFormatter = readFormatter("observer");

        assert.strictEqual(
            fnFormatter("", "Integration Observer", "Reference Observer"),
            "Integration Observer",
            "integration-owned name is used for empty PERNR"
        );
        assert.strictEqual(
            fnFormatter("00001234", "Integration Observer", "Reference Observer"),
            "Reference Observer",
            "reference-backed fullname stays canonical when PERNR exists"
        );
    });

    QUnit.test("observed read-only text falls back to integration name when pernr is empty", function (assert) {
        var fnFormatter = readFormatter("observed");

        assert.strictEqual(
            fnFormatter("", "Integration Observed", "Reference Observed"),
            "Integration Observed",
            "integration-owned name is used for empty PERNR"
        );
        assert.strictEqual(
            fnFormatter("00004321", "Integration Observed", "Reference Observed"),
            "Reference Observed",
            "reference-backed fullname stays canonical when PERNR exists"
        );
    });
});
