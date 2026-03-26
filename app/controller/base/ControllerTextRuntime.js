sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime"
], function (ModelContracts, ControllerModelRuntime) {
    "use strict";

    var I18N_MODEL = ModelContracts.MODELS.I18N;

    function resolveBundle(oController) {
        var oI18nModel = ControllerModelRuntime.model(oController, I18N_MODEL, true);
        return oI18nModel && typeof oI18nModel.getResourceBundle === "function"
            ? oI18nModel.getResourceBundle()
            : null;
    }

    function getText(oController, sTextKey, aArgs, sFallback) {
        var oBundle = resolveBundle(oController);
        if (!sTextKey || !oBundle || typeof oBundle.getText !== "function") {
            return String(sFallback || sTextKey || "");
        }
        try {
            return String(oBundle.getText(sTextKey, Array.isArray(aArgs) ? aArgs : []) || sFallback || sTextKey || "");
        } catch (_bundleError) {
            return String(sFallback || sTextKey || "");
        }
    }

    function resolve(sTextKey, oController, aArgs, sFallback) {
        return getText(oController, sTextKey, aArgs || [], sFallback || sTextKey);
    }

    return Object.freeze({
        resolve: resolve,
        resolveBundle: resolveBundle,
        getText: getText
    });
});
