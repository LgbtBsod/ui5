sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap_ui5/service/backend/BackendAdapter",
    "sap_ui5/util/ThemePhilosophy",
    "sap/m/MessageToast",
    "sap/m/MessageBox"
], function (Controller, BackendAdapter, ThemePhilosophy, MessageToast, MessageBox) {
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
            var sTheme = window.localStorage.getItem(THEME_STORAGE_KEY);
            if (!sTheme) {
                // Light is the default baseline theme and is persisted per-site in browser storage.
                window.localStorage.setItem(THEME_STORAGE_KEY, LIGHT_THEME);
                return LIGHT_THEME;
            }
            return sTheme;
        },

        isDarkThemeEnabled: function () {
            return _isDarkTheme(this.getCurrentTheme());
        },

        applyStoredTheme: function () {
            var sTheme = this.getCurrentTheme();
            this._applyTheme(sTheme);
            return {
                isDark: _isDarkTheme(sTheme),
                theme: sTheme,
                meta: ThemePhilosophy.getMeta(sTheme)
            };
        },

        toggleTheme: function () {
            var sNextTheme = this.isDarkThemeEnabled() ? LIGHT_THEME : DARK_THEME;
            this._applyTheme(sNextTheme);
            window.localStorage.setItem(THEME_STORAGE_KEY, sNextTheme);
            return {
                isDark: _isDarkTheme(sNextTheme),
                theme: sNextTheme,
                meta: ThemePhilosophy.getMeta(sNextTheme)
            };
        },


        /**
         * Converts Base64 text into hexadecimal string.
         * Useful for debug payloads and lock/session tracing in test mode.
         */
        base64ToHex: function (sBase64) {
            if (!sBase64) {
                return "";
            }

            var sBinary = atob(String(sBase64));
            var aHex = [];
            for (var i = 0; i < sBinary.length; i += 1) {
                aHex.push(sBinary.charCodeAt(i).toString(16).padStart(2, "0"));
            }
            return aHex.join("");
        },


        releaseLock: function (sObjectId, sSessionId, mOptions) {
            if (!sObjectId || !sSessionId) {
                return Promise.resolve();
            }
            return BackendAdapter.lockRelease(sObjectId, sSessionId, mOptions || { trySave: true }).catch(function () {
                return null;
            });
        },

        setLockUiState: function (oStateModel, sState, sText) {
            if (!oStateModel) {
                return;
            }
            oStateModel.setProperty("/lockOperationState", sState || "IDLE");
            oStateModel.setProperty("/lockOperationText", sText || "");
        },

        setLockPending: function (oStateModel, bPending) {
            if (!oStateModel) {
                return;
            }
            oStateModel.setProperty("/lockOperationPending", !!bPending);
        },


        /**
         * Reusable row-deletion helper for table handlers in controllers.
         */
        deleteRowFromEvent: function (oEvent, sModelName, sCollectionPath) {
            var oCtx = oEvent && oEvent.getSource && oEvent.getSource().getBindingContext(sModelName);
            if (!oCtx) {
                return { deleted: false };
            }

            var iIndex = Number(oCtx.getPath().split("/").pop());
            var oModel = this.getModel(sModelName);
            var aItems = oModel.getProperty(sCollectionPath) || [];
            if (!Number.isInteger(iIndex) || iIndex < 0 || iIndex >= aItems.length) {
                return { deleted: false };
            }

            var aNext = aItems.slice();
            aNext.splice(iIndex, 1);
            oModel.setProperty(sCollectionPath, aNext);
            return { deleted: true, index: iIndex };
        },


        /**
         * Generic async wrapper for busy/loading flags.
         * Prevents duplicated try/finally blocks in feature controllers.
         */


        /**
         * Unified i18n toast helper to reduce repeated MessageToast boilerplate in controllers.
         */
        showI18nToast: function (sI18nKey, aArgs) {
            MessageToast.show(this.getResourceBundle().getText(sI18nKey, aArgs || []));
        },

        /**
         * Unified i18n message-box error helper for consistent UX and auditing readability.
         */
        showI18nError: function (sI18nKey, aArgs) {
            MessageBox.error(this.getResourceBundle().getText(sI18nKey, aArgs || []));
        },

        runWithStateFlag: function (oStateModel, sPath, fnTask) {
            if (!oStateModel || !sPath || typeof fnTask !== "function") {
                return Promise.resolve(null);
            }
            oStateModel.setProperty(sPath, true);
            return Promise.resolve().then(fnTask).finally(function () {
                oStateModel.setProperty(sPath, false);
            });
        },

        _applyTheme: function (sTheme) {
            var bDark = _isDarkTheme(sTheme);
            sap.ui.getCore().applyTheme(sTheme);
            document.body.classList.toggle("appDark", bDark);
            document.body.classList.toggle("appLight", !bDark);
            document.body.classList.toggle("lightMode", !bDark);
            document.documentElement.classList.toggle("light-mode", !bDark);

            var oMeta = ThemePhilosophy.getMeta(sTheme);
            document.body.classList.remove("themeLifestyleClarity", "themeLifestyleNightOps");
            document.body.classList.add(oMeta.lifestyleClass);
        }
    });
});
