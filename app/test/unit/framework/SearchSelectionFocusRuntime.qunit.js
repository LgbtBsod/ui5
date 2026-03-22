sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionRuntime",
    "sap/ui/thirdparty/jquery"
], function (SearchSelectionRuntime, jQuery) {
    "use strict";

    function attachFocusSpy(oNode) {
        oNode.dataset.focused = "";
        Object.defineProperty(oNode, "focus", {
            configurable: true,
            value: function () {
                oNode.dataset.focused = "true";
            }
        });
        return oNode;
    }

    function createControllerFixture(sMarkup) {
        var oHost = document.createElement("div");
        oHost.innerHTML = sMarkup;
        document.body.appendChild(oHost);
        return {
            host: oHost,
            controller: {
                getView: function () {
                    return {
                        getDomRef: function () {
                            return oHost;
                        }
                    };
                },
                byId: function () {
                    return null;
                }
            }
        };
    }

    QUnit.module("SearchSelectionRuntime focus methods", {
        afterEach: function () {
            jQuery("div[data-qunit-search-focus='true']").remove();
        }
    });

    QUnit.test("focusSearchToolbar stays scoped to the current view", function (assert) {
        var oOutside = document.createElement("input");
        var oFixture;
        var oTargetControl;

        oOutside.id = "outside-backendTopInput-inner";
        attachFocusSpy(oOutside);
        document.body.appendChild(oOutside);

        oFixture = createControllerFixture(
            '<div data-qunit-search-focus="true">' +
                '<input id="fixture-backendTopInput-inner" />' +
            '</div>'
        );
        oFixture.host.setAttribute("data-qunit-search-focus", "true");
        attachFocusSpy(oFixture.host.querySelector("#fixture-backendTopInput-inner"));
        oTargetControl = {
            focus: function () {
                oFixture.host.querySelector("#fixture-backendTopInput-inner").dataset.focused = "true";
            },
            getDomRef: function () {
                return oFixture.host.querySelector("#fixture-backendTopInput-inner");
            }
        };
        oFixture.controller.byId = function (sId) {
            return sId === "backendTopInput" ? oTargetControl : null;
        };

        SearchSelectionRuntime.focusSearchToolbar(oFixture.controller);

        assert.strictEqual(oFixture.host.querySelector("#fixture-backendTopInput-inner").dataset.focused, "true", "Focus stayed inside the current search view");
        assert.notStrictEqual(oOutside.dataset.focused, "true", "Outside node was not targeted");
        oOutside.remove();
        oFixture.host.remove();
    });

    QUnit.test("focusSearchFilters stays scoped to the current view", function (assert) {
        var oOutside = document.createElement("button");
        var oFixture;
        var oTargetControl;

        oOutside.id = "outside-searchSmartFilterBar-btnGo";
        attachFocusSpy(oOutside);
        document.body.appendChild(oOutside);

        oFixture = createControllerFixture(
            '<div data-qunit-search-focus="true">' +
                '<button id="fixture-searchSmartFilterBar-btnGo"></button>' +
            '</div>'
        );
        oFixture.host.setAttribute("data-qunit-search-focus", "true");
        attachFocusSpy(oFixture.host.querySelector("#fixture-searchSmartFilterBar-btnGo"));
        oTargetControl = {
            focus: function () {
                oFixture.host.querySelector("#fixture-searchSmartFilterBar-btnGo").dataset.focused = "true";
            },
            getDomRef: function () {
                return oFixture.host.querySelector("#fixture-searchSmartFilterBar-btnGo");
            }
        };
        oFixture.controller.byId = function (sId) {
            return sId === "searchSmartFilterBar" ? oTargetControl : null;
        };

        SearchSelectionRuntime.focusSearchFilters(oFixture.controller);

        assert.strictEqual(oFixture.host.querySelector("#fixture-searchSmartFilterBar-btnGo").dataset.focused, "true", "Filter focus stayed inside the current search view");
        assert.notStrictEqual(oOutside.dataset.focused, "true", "Outside filter node was not targeted");
        oOutside.remove();
        oFixture.host.remove();
    });

    QUnit.test("focusSearchResults falls back to a view-scoped results target", function (assert) {
        var oFixture = createControllerFixture(
            '<div data-qunit-search-focus="true">' +
                '<div class="searchResultsTable">' +
                    '<button id="fixture-results-row" role="row"></button>' +
                '</div>' +
            '</div>'
        );

        oFixture.host.setAttribute("data-qunit-search-focus", "true");
        attachFocusSpy(oFixture.host.querySelector("#fixture-results-row"));
        SearchSelectionRuntime.focusSearchResults(oFixture.controller);

        assert.strictEqual(oFixture.host.querySelector("#fixture-results-row").dataset.focused, "true", "Results focus stayed inside the current search view");
        oFixture.host.remove();
    });
});
