sap.ui.define([
    "sap/ui/core/mvc/Controller"
], function (Controller) {
    "use strict";

    var THEME_STORAGE_KEY = "sap_ui5_theme";
    var DARK_THEME = "dark";
    var LIGHT_THEME = "light";

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
            return this.isDarkThemeEnabled();
        },

        isDarkThemeEnabled: function () {
            return document.body.classList.contains("appDark");
        },

        applyStoredTheme: function () {
            var sTheme = window.localStorage.getItem(THEME_STORAGE_KEY) || DARK_THEME;
            this._applyTheme(sTheme);
            return sTheme;
        },

        toggleTheme: function () {
            var sNextTheme = this.isDarkThemeEnabled() ? LIGHT_THEME : DARK_THEME;
            this._applyTheme(sNextTheme);
            window.localStorage.setItem(THEME_STORAGE_KEY, sNextTheme);
            return sNextTheme === DARK_THEME;
        },

        _applyTheme: function (sTheme) {
            var bDark = sTheme !== LIGHT_THEME;
            document.body.classList.toggle("appDark", bDark);
            document.body.classList.toggle("appLight", !bDark);
            document.documentElement.classList.toggle("light-mode", !bDark);
        }
    });
});
