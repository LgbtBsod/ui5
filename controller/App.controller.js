sap.ui.define([
    "sap_ui5/controller/Base.controller",
    "sap/ui/model/json/JSONModel"
], function (BaseController, JSONModel) {
    "use strict";

    return BaseController.extend("sap_ui5.controller.App", {

        onInit: function () {
            this.setModel(new JSONModel({
                isDark: this.isDarkThemeEnabled()
            }), "appView");
        },

        onToggleTheme: function () {
            var bIsDark = this.toggleTheme();
            this.getView().getModel("appView").setProperty("/isDark", bIsDark);
        }
    });
});
