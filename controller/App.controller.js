sap.ui.define([
    "sap_ui5/controller/Base"
], function (BaseController) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.App", {

        onToggleTheme: function () {
            this.toggleTheme();
        }

    });
});
