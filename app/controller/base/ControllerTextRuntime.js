sap.ui.define([
    "checklist/app/service/framework/EffectTextResolver"
], function (EffectTextResolver) {
    "use strict";

    function resolveBundle(oController) {
        return EffectTextResolver.resolveBundle(oController);
    }

    function getText(oController, sTextKey, aArgs, sFallback) {
        return EffectTextResolver.getText(oController, sTextKey, aArgs, sFallback);
    }

    return {
        resolveBundle: resolveBundle,
        getText: getText
    };
});
