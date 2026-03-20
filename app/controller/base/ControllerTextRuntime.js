sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectTextResolver"
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
