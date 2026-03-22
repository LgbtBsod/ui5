sap.ui.define([
    "sap/ui/core/Core",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (Core, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function getCore() {
        return Core || null;
    }

    function attachThemeChanged(fnHandler) {
        var oCore = getCore();
        if (oCore && typeof oCore[METHODS.ATTACH_THEME_CHANGED] === TYPE_FUNCTION && typeof fnHandler === TYPE_FUNCTION) {
            oCore[METHODS.ATTACH_THEME_CHANGED](fnHandler);
            return true;
        }
        return false;
    }

    function detachThemeChanged(fnHandler) {
        var oCore = getCore();
        if (oCore && typeof oCore[METHODS.DETACH_THEME_CHANGED] === TYPE_FUNCTION && typeof fnHandler === TYPE_FUNCTION) {
            oCore[METHODS.DETACH_THEME_CHANGED](fnHandler);
            return true;
        }
        return false;
    }

    function getStaticAreaRef() {
        var oCore = getCore();
        return oCore && typeof oCore[METHODS.GET_STATIC_AREA_REF] === TYPE_FUNCTION ? oCore[METHODS.GET_STATIC_AREA_REF]() : null;
    }

    function getLanguageTag() {
        var oCore = getCore();
        var oConfiguration = oCore && typeof oCore[METHODS.GET_CONFIGURATION] === TYPE_FUNCTION ? oCore[METHODS.GET_CONFIGURATION]() : null;
        var oLanguageTag = oConfiguration && typeof oConfiguration[METHODS.GET_LANGUAGE_TAG] === TYPE_FUNCTION ? oConfiguration[METHODS.GET_LANGUAGE_TAG]() : null;
        return oLanguageTag && typeof oLanguageTag[METHODS.TO_STRING] === TYPE_FUNCTION ? String(oLanguageTag[METHODS.TO_STRING]() || "") : "";
    }

    function getModel(sName) {
        var oCore = getCore();
        return oCore && typeof oCore[METHODS.GET_MODEL] === TYPE_FUNCTION ? oCore[METHODS.GET_MODEL](sName) : null;
    }

    function getI18nBundle() {
        var oModel = getModel("i18n");
        return oModel && typeof oModel[METHODS.GET_RESOURCE_BUNDLE] === TYPE_FUNCTION ? oModel[METHODS.GET_RESOURCE_BUNDLE]() : null;
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
