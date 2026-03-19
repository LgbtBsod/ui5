sap.ui.define([
    "sap/ui/core/Core"
], function (Core) {
    "use strict";

    function getCore() {
        return Core || null;
    }

    function attachThemeChanged(fnHandler) {
        var oCore = getCore();
        if (oCore && typeof oCore.attachThemeChanged === "function" && typeof fnHandler === "function") {
            oCore.attachThemeChanged(fnHandler);
            return true;
        }
        return false;
    }

    function detachThemeChanged(fnHandler) {
        var oCore = getCore();
        if (oCore && typeof oCore.detachThemeChanged === "function" && typeof fnHandler === "function") {
            oCore.detachThemeChanged(fnHandler);
            return true;
        }
        return false;
    }

    function getStaticAreaRef() {
        var oCore = getCore();
        return oCore && typeof oCore.getStaticAreaRef === "function" ? oCore.getStaticAreaRef() : null;
    }

    function getLanguageTag() {
        var oCore = getCore();
        var oConfiguration = oCore && typeof oCore.getConfiguration === "function" ? oCore.getConfiguration() : null;
        var oLanguageTag = oConfiguration && typeof oConfiguration.getLanguageTag === "function" ? oConfiguration.getLanguageTag() : null;
        return oLanguageTag && typeof oLanguageTag.toString === "function" ? String(oLanguageTag.toString() || "") : "";
    }

    function getModel(sName) {
        var oCore = getCore();
        return oCore && typeof oCore.getModel === "function" ? oCore.getModel(sName) : null;
    }

    function getI18nBundle() {
        var oModel = getModel("i18n");
        return oModel && typeof oModel.getResourceBundle === "function" ? oModel.getResourceBundle() : null;
    }

    return {
        attachThemeChanged: attachThemeChanged,
        detachThemeChanged: detachThemeChanged,
        getCore: getCore,
        getI18nBundle: getI18nBundle,
        getLanguageTag: getLanguageTag,
        getModel: getModel,
        getStaticAreaRef: getStaticAreaRef
    };
});
