sap.ui.define([
    "sap/ui/core/mvc/Controller"
], function (Controller) {
    "use strict";

    var THEME_STORAGE_KEY = "sap_ui5_theme";
    var LIGHT_THEME = "sap_fiori_3";
    var DARK_THEME = "sap_fiori_3_dark";

    function _isDarkTheme(sTheme) {
        return sTheme === DARK_THEME;
    }

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

        getCurrentTheme: function () {
            return window.localStorage.getItem(THEME_STORAGE_KEY) || DARK_THEME;
        },

        isDarkThemeEnabled: function () {
            return _isDarkTheme(this.getCurrentTheme());
        },

        applyStoredTheme: function () {
            var sTheme = this.getCurrentTheme();
            this._applyTheme(sTheme);
            return {
                isDark: _isDarkTheme(sTheme),
                theme: sTheme
            };
        },

        toggleTheme: function () {
            var sNextTheme = this.isDarkThemeEnabled() ? LIGHT_THEME : DARK_THEME;
            this._applyTheme(sNextTheme);
            window.localStorage.setItem(THEME_STORAGE_KEY, sNextTheme);
            return {
                isDark: _isDarkTheme(sNextTheme),
                theme: sNextTheme
            };
        },

        _applyTheme: function (sTheme) {
            var bDark = _isDarkTheme(sTheme);
            sap.ui.getCore().applyTheme(sTheme);
            document.body.classList.toggle("appDark", bDark);
            document.body.classList.toggle("appLight", !bDark);
            document.body.classList.toggle("lightMode", !bDark);
            document.documentElement.classList.toggle("light-mode", !bDark);
        }
    });
});
