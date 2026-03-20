sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Ui5RuntimeFacade"
], function (Ui5RuntimeFacade) {
    "use strict";

    function resolveCoreBundle() {
        try {
            return Ui5RuntimeFacade.getI18nBundle();
        } catch (_e) {
            return null;
        }
    }

    function resolveBundle(oController) {
        if (!oController) {
            return resolveCoreBundle();
        }
        if (typeof oController.getResourceBundle === "function") {
            try {
                return oController.getResourceBundle() || resolveCoreBundle();
            } catch (_e) {
                return resolveCoreBundle();
            }
        }
        if (typeof oController.getModel !== "function") {
            return resolveCoreBundle();
        }
        try {
            return (oController.getModel("i18n") && oController.getModel("i18n").getResourceBundle && oController.getModel("i18n").getResourceBundle()) || resolveCoreBundle();
        } catch (_e2) {
            return resolveCoreBundle();
        }
    }

    function getText(oController, sTextKey, aArgs, sFallback) {
        var oBundle;
        var sKey = String(sTextKey || "");
        var aResolvedArgs = Array.isArray(aArgs) ? aArgs : [];
        var sResolvedText = "";
        if (!sKey) {
            return String(sFallback || "");
        }
        oBundle = resolveBundle(oController);
        try {
            if (oBundle && oBundle.getText) {
                sResolvedText = String(oBundle.getText(sKey, aResolvedArgs) || "");
                if (sResolvedText && sResolvedText !== sKey) {
                    return sResolvedText;
                }
            }
            if (oBundle && oBundle.hasText && oBundle.hasText(sKey) && oBundle.getText) {
                return String(oBundle.getText(sKey, aResolvedArgs) || "");
            }
        } catch (_e3) {
            return String(sFallback || sKey);
        }
        return String(sFallback || sKey);
    }

    function resolve(sTextKey, oController) {
        return getText(oController, sTextKey, [], sTextKey);
    }

    return {
        resolve: resolve,
        resolveBundle: resolveBundle,
        getText: getText
    };
});
