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

        toggleTheme: function () {
            var oConfig = sap.ui.getCore().getConfiguration();
            var sCurrentTheme = oConfig.getTheme();
            var sNextTheme = sCurrentTheme === "sap_fiori_3" ? "sap_fiori_3_dark" : "sap_fiori_3";

            sap.ui.getCore().applyTheme(sNextTheme);
            document.body.classList.toggle("appDark", sNextTheme === "sap_fiori_3_dark");
        }
    });
});
