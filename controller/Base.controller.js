sap.ui.define([
    "sap/ui/core/mvc/Controller"
], function (Controller) {
    "use strict";

    return Controller.extend("sap_ui5.controller.Base", {

        getRouter: function () {
            return this.getOwnerComponent().getRouter();
        },

        getModel: function (sName) {
            return this.getOwnerComponent().getModel(sName);
        },

        setModel: function (oModel, sName) {
            return this.getView().setModel(oModel, sName);
        },

        getResourceBundle: function () {
            return this.getOwnerComponent().getModel("i18n").getResourceBundle();
        },

        navTo: function (sRoute, oParameters, bReplace) {
            this.getRouter().navTo(sRoute, oParameters || {}, bReplace);
        },

        attachRouteMatched: function (sRouteName, fnHandler) {
            this.getRouter().getRoute(sRouteName).attachPatternMatched(fnHandler, this);
        },

        isDarkAccentEnabled: function () {
            return document.body.classList.contains("appDark");
        },

        toggleTheme: function () {
            var bIsDark = !this.isDarkAccentEnabled();
            document.body.classList.toggle("appDark", bIsDark);
            return bIsDark;
        }
    });
});
