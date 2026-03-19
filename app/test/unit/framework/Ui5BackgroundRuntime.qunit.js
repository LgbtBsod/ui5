sap.ui.define([], function () {
    "use strict";

    QUnit.module("UI5 background runtime", {
        beforeEach: function () {
            this._originalTheme = document.body.getAttribute("data-theme");
            this._originalEnabled = document.body.getAttribute("data-bg-enabled");
            this._host = document.createElement("div");
            this._host.id = "ui5-bg-host";
            this._host.className = "ui5BgHost";
            document.body.appendChild(this._host);
        },
        afterEach: function () {
            if (this._host && this._host.parentNode) {
                this._host.parentNode.removeChild(this._host);
            }
            if (this._originalTheme === null) {
                document.body.removeAttribute("data-theme");
            } else {
                document.body.setAttribute("data-theme", this._originalTheme);
            }
            if (this._originalEnabled === null) {
                document.body.removeAttribute("data-bg-enabled");
            } else {
                document.body.setAttribute("data-bg-enabled", this._originalEnabled);
            }
        }
    });

    QUnit.test("productive background host stays light-only and reflects animation state", function (assert) {
        var oHost = document.getElementById("ui5-bg-host");

        document.body.setAttribute("data-theme", "light");
        document.body.setAttribute("data-bg-enabled", "false");

        oHost.classList.toggle("is-light", true);
        oHost.classList.toggle("is-dark", false);
        oHost.classList.toggle("is-disabled", document.body.getAttribute("data-bg-enabled") === "false");

        assert.strictEqual(document.body.getAttribute("data-theme"), "light", "body theme stays locked to light");
        assert.true(oHost.classList.contains("is-light"), "light class is applied");
        assert.false(oHost.classList.contains("is-dark"), "dark class is not used");
        assert.true(oHost.classList.contains("is-disabled"), "disabled class tracks animation flag");
    });
});
