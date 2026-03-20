sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectTextResolver"
], function (EffectTextResolver) {
    "use strict";

    function resolveText(oController, sTextKey, aArgs, sFallback) {
        return EffectTextResolver.getText(
            oController,
            sTextKey,
            aArgs || [],
            sFallback || sTextKey || ""
        );
    }

    function createResolver(oController) {
        return function (sKey, aArgs, sFallback) {
            return resolveText(oController, sKey, aArgs || [], sFallback || sKey);
        };
    }

    return Object.freeze({
        resolveText: resolveText,
        createResolver: createResolver
    });
});
