sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/controls/AppShellHeader"
], function (AppShellHeader) {
    "use strict";

    QUnit.module("AppShellHeader");

    QUnit.test("updates internal toolbar content when properties change after init", function (assert) {
        var oControl = new AppShellHeader({
            productName: "Initial",
            userLabel: "User A"
        });

        oControl.setProductName("Updated Product");
        oControl.setUserLabel("User B");
        oControl.setUserTooltip("Tooltip B");

        assert.strictEqual(oControl._oProductTitle.getText(), "Updated Product", "product title stays reactive");
        assert.strictEqual(oControl._oUserButton.getText(), "User B", "user button label stays reactive");
        assert.strictEqual(oControl._oUserButton.getTooltip(), "Tooltip B", "user tooltip stays reactive");

        oControl.destroy();
    });
});
