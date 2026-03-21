sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/features/search/runtime/SearchStickyOffsetRuntime"
], function (SearchStickyOffsetRuntime) {
    "use strict";

    QUnit.module("SearchStickyOffsetRuntime", {
        afterEach: function () {
            Array.prototype.slice.call(document.querySelectorAll("[data-qunit-sticky-offset='true']")).forEach(function (oNode) {
                oNode.remove();
            });
            document.documentElement.style.removeProperty("--app-shell-offset");
        }
    });

    QUnit.test("resolveShellHeaderOffset respects shell and scroll host geometry", function (assert) {
        var oShellHeader = document.createElement("div");
        var oScrollHost = document.createElement("div");
        var iOffset;

        oShellHeader.className = "appShellHeader";
        oShellHeader.setAttribute("data-qunit-sticky-offset", "true");
        oScrollHost.setAttribute("data-qunit-sticky-offset", "true");
        document.documentElement.style.setProperty("--app-shell-offset", "112px");

        oScrollHost.getBoundingClientRect = function () {
            return {
                top: 40
            };
        };

        document.body.appendChild(oShellHeader);
        document.body.appendChild(oScrollHost);

        iOffset = SearchStickyOffsetRuntime.resolveShellHeaderOffset(24, 8, oScrollHost);

        assert.strictEqual(iOffset, 80, "Sticky offset is derived from shell bottom, host top, and padding");
    });

    QUnit.test("resolveShellHeaderOffset falls back to minimum when shell geometry is unavailable", function (assert) {
        var iOffset = SearchStickyOffsetRuntime.resolveShellHeaderOffset(24, 8, null);

        assert.strictEqual(iOffset, 24, "Minimum sticky offset is preserved without shell geometry");
    });
});
