sap.ui.define([], function () {
    "use strict";

    function resolveBundle(oController) {
        if (!oController) {
            return null;
        }
        if (typeof oController.getResourceBundle === "function") {
            try {
                return oController.getResourceBundle();
            } catch (_e) {
                return null;
            }
        }
        if (typeof oController.getModel !== "function") {
            return null;
        }
        try {
            return oController.getModel("i18n") && oController.getModel("i18n").getResourceBundle && oController.getModel("i18n").getResourceBundle();
        } catch (_e2) {
            return null;
        }
    }

    function getText(oController, sTextKey, aArgs, sFallback) {
        var oBundle;
        var sKey = String(sTextKey || "");
        if (!sKey) {
            return String(sFallback || "");
        }
        oBundle = resolveBundle(oController);
        try {
            if (oBundle && oBundle.hasText && oBundle.hasText(sKey) && oBundle.getText) {
                return oBundle.getText(sKey, Array.isArray(aArgs) ? aArgs : []);
            }
            if (oBundle && !oBundle.hasText && oBundle.getText) {
                return oBundle.getText(sKey, Array.isArray(aArgs) ? aArgs : []);
            }
        } catch (_e3) {
            return String(sFallback || sKey);
        }
        return String(sFallback || sKey);
    }

    return {
        resolveBundle: resolveBundle,
        getText: getText
    };
});
