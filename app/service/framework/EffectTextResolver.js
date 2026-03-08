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
