sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel"
], function (BaseController, JSONModel) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.App", {

        onInit: function () {
            var oApplied = this.applyStoredTheme();
            this.setModel(new JSONModel({
                isDark: !!oApplied.isDark
            }), "appView");
        },

        onToggleTheme: function () {
            var oResult = this.toggleTheme();
            this.getView().getModel("appView").setProperty("/isDark", !!oResult.isDark);
        }
    });
});
