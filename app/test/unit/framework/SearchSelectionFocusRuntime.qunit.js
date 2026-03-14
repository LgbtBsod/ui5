sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchSelectionFocusRuntime",
    "sap/ui/thirdparty/jquery"
], function (SearchSelectionFocusRuntime, jQuery) {
    "use strict";

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

    QUnit.module("SearchSelectionFocusRuntime", {
        afterEach: function () {
            jQuery("div[data-qunit-search-focus='true']").remove();
        }
    });

    QUnit.test("focusSearchToolbar stays scoped to the current view", function (assert) {
        var done = assert.async();
        var oOutside = document.createElement("input");
        var oFixture;

        oOutside.id = "outside-backendTopInput-inner";
        document.body.appendChild(oOutside);

        oFixture = createControllerFixture(
            '<div data-qunit-search-focus="true">' +
                '<input id="fixture-backendTopInput-inner" />' +
            '</div>'
        );
        oFixture.host.setAttribute("data-qunit-search-focus", "true");

        SearchSelectionFocusRuntime.focusSearchToolbar(oFixture.controller);

        setTimeout(function () {
            assert.strictEqual(document.activeElement.id, "fixture-backendTopInput-inner", "Focus stayed inside the current search view");
            oOutside.remove();
            oFixture.host.remove();
            done();
        }, 0);
    });

    QUnit.test("focusSearchFilters stays scoped to the current view", function (assert) {
        var done = assert.async();
        var oOutside = document.createElement("button");
        var oFixture;

        oOutside.id = "outside-searchSmartFilterBar-btnGo";
        document.body.appendChild(oOutside);

        oFixture = createControllerFixture(
            '<div data-qunit-search-focus="true">' +
                '<button id="fixture-searchSmartFilterBar-btnGo"></button>' +
            '</div>'
        );
        oFixture.host.setAttribute("data-qunit-search-focus", "true");

        SearchSelectionFocusRuntime.focusSearchFilters(oFixture.controller, function () {
            return null;
        });

        setTimeout(function () {
            assert.strictEqual(document.activeElement.id, "fixture-searchSmartFilterBar-btnGo", "Filter focus stayed inside the current search view");
            oOutside.remove();
            oFixture.host.remove();
            done();
        }, 0);
    });
});
