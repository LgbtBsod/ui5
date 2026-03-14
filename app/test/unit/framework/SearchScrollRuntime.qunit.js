sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchScrollRuntime"
], function (SearchScrollRuntime) {
    "use strict";

    function createStateModel() {
        var oData = {};
        return {
            getProperty: function (sPath) {
                return oData[sPath];
            },
            setProperty: function (sPath, vValue) {
                oData[sPath] = vValue;
            }
        };
    }

    function createControllerFixture() {
        var oScrollHost = document.createElement("div");
        var oViewRoot = document.createElement("div");
        var oStateModel = createStateModel();

        oScrollHost.style.height = "120px";
        oScrollHost.style.overflow = "auto";
        oScrollHost.setAttribute("data-qunit-search-scroll", "true");

        oViewRoot.innerHTML = '<div style="height: 600px;"></div>';
        oScrollHost.appendChild(oViewRoot);
        document.body.appendChild(oScrollHost);

        return {
            scrollHost: oScrollHost,
            viewRoot: oViewRoot,
            controller: {
                getView: function () {
                    return {
                        getDomRef: function () {
                            return oViewRoot;
                        }
                    };
                },
                getModel: function (sName) {
                    return sName === "state" ? oStateModel : null;
                }
            }
        };
    }

    QUnit.module("SearchScrollRuntime", {
        afterEach: function () {
            Array.prototype.slice.call(document.querySelectorAll("[data-qunit-search-scroll='true']")).forEach(function (oNode) {
                oNode.remove();
            });
        }
    });

    QUnit.test("capture and restore keep search scroll position across search-detail-search transition intent", function (assert) {
        var done = assert.async();
        var oFixture = createControllerFixture();

        oFixture.scrollHost.scrollTop = 180;
        SearchScrollRuntime.captureSearchScrollPosition(oFixture.controller);

        oFixture.scrollHost.scrollTop = 0;
        SearchScrollRuntime.restoreSearchScrollPosition(oFixture.controller, {
            resolveToolbarDom: function () {
                return null;
            },
            syncViewportLayout: function () {}
        });

        setTimeout(function () {
            assert.strictEqual(oFixture.scrollHost.scrollTop, 180, "Search scroll position is restored");
            done();
        }, 50);
    });
});
