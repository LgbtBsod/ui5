sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchFormatters"
], function (SearchFormatters) {
    "use strict";

    function createController(mTexts) {
        return {
            getResourceBundle: function () {
                return {
                    getText: function (sKey) {
                        return Object.prototype.hasOwnProperty.call(mTexts, sKey) ? mTexts[sKey] : "";
                    }
                };
            }
        };
    }

    QUnit.module("SearchFormatters");

    QUnit.test("formatSearchModeChipText falls back to EXACT for unknown mode", function (assert) {
        var oController = createController({
            searchModeLabel: "Mode",
            searchModeExact: "Exact"
        });
        assert.strictEqual(SearchFormatters.formatSearchModeChipText(oController, "bogus"), "Mode: Exact");
    });

    QUnit.test("formatSearchModeChipText resolves LOOSE mode", function (assert) {
        var oController = createController({
            searchModeLabel: "Mode",
            searchModeLoose: "Loose"
        });
        assert.strictEqual(SearchFormatters.formatSearchModeChipText(oController, "loose"), "Mode: Loose");
    });

    QUnit.test("formatSearchResultsCompactText hides count when there are no rows", function (assert) {
        var oController = createController({ resultsLabel: "Results" });
        assert.strictEqual(SearchFormatters.formatSearchResultsCompactText(oController, 12, false), "Results");
    });

    QUnit.test("formatSearchResultsCompactText appends count when rows are present", function (assert) {
        var oController = createController({ resultsLabel: "Results" });
        assert.strictEqual(SearchFormatters.formatSearchResultsCompactText(oController, 12, true), "Results: 12");
    });

    QUnit.test("formatSearchSelectionSummary reports none when selection is empty", function (assert) {
        var oController = createController({ searchSelectionNone: "None selected" });
        assert.strictEqual(SearchFormatters.formatSearchSelectionSummary(oController, 0, ""), "None selected");
    });

    QUnit.test("formatSearchSelectionSummary shows the primary id for a single selection", function (assert) {
        var oController = createController({ searchSelectionPrimaryPrefix: "Selected" });
        assert.strictEqual(SearchFormatters.formatSearchSelectionSummary(oController, 1, "CHK-1"), "Selected: CHK-1");
    });

    QUnit.test("formatSearchSelectionSummary shows the unit count for multiple selections", function (assert) {
        var oController = createController({ searchSelectionUnits: "items" });
        assert.strictEqual(SearchFormatters.formatSearchSelectionSummary(oController, 3, ""), "3 items");
    });
});
