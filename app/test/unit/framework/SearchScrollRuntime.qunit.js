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
        var oScrollHost;
        var oViewRoot;
        var oStateModel = createStateModel();

        oScrollHost = document.createElement("div");
        oScrollHost.setAttribute("data-qunit-search-scroll", "true");
        oScrollHost.style.height = "120px";
        oScrollHost.style.overflow = "auto";
        Object.defineProperty(oScrollHost, "clientHeight", { value: 120, configurable: true });
        Object.defineProperty(oScrollHost, "scrollHeight", { value: 600, configurable: true });
        oScrollHost.scrollTop = 0;

        oViewRoot = document.createElement("div");
        oViewRoot.setAttribute("data-qunit-search-scroll", "true");
        oViewRoot.style.height = "600px";
        oScrollHost.appendChild(oViewRoot);
        document.body.appendChild(oScrollHost);

        return {
            scrollHost: oScrollHost,
            viewRoot: oViewRoot,
            controller: {
                getModel: function (sName) {
                    return sName === "state" ? oStateModel : null;
                },
                getView: function () {
                    return {
                        getDomRef: function () {
                            return oViewRoot;
                        },
                        getModel: function (sName) {
                            return sName === "state" ? oStateModel : null;
                        }
                    };
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
        var fnOriginalRequestFrame = window.requestAnimationFrame;
        var fnOriginalCancelFrame = window.cancelAnimationFrame;

        window.requestAnimationFrame = function (fnWork) {
            return window.setTimeout(fnWork, 0);
        };
        window.cancelAnimationFrame = function (iFrameId) {
            window.clearTimeout(iFrameId);
        };

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
            window.requestAnimationFrame = fnOriginalRequestFrame;
            window.cancelAnimationFrame = fnOriginalCancelFrame;
            done();
        }, 30);
    });
});
