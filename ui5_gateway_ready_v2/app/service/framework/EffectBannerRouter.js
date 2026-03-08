sap.ui.define([
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/EffectActionRouting"
], function (FeedbackBannerRuntime, EffectActionRouting) {
    "use strict";

    function readStateModel(oController) {
        return oController && oController.getModel ? oController.getModel("state") : null;
    }

    function resolveTextKey(oEffect, sFallbackTextKey) {
        var oPayload = (oEffect && oEffect.payload) || {};
        return (oEffect && oEffect.textKey) || oPayload.messageKey || String(sFallbackTextKey || "");
    }

    function resolveTextValue(sTextKey, oController, oEffect, mOptions) {
        var fnResolveTextKey = mOptions && mOptions.resolveTextKey;
        if (!sTextKey || typeof fnResolveTextKey !== "function") {
            return "";
        }
        return String(fnResolveTextKey(sTextKey, oController, oEffect) || "");
    }

    function handleEffect(oController, oEffect, oRuntimeOptions, mOptions) {
        var oState = readStateModel(oController);
        var oLocalOptions = mOptions || {};
        var sAction = EffectActionRouting.normalizeEffectVerb(oEffect && oEffect.action);

        if (sAction === "dispatch") {
            return EffectActionRouting.dispatchEffectAction(oController, oRuntimeOptions, oEffect);
        }
        if (sAction === "clear") {
            return FeedbackBannerRuntime.clearFromEffect(oState, oEffect);
        }

        var sTextKey = resolveTextKey(oEffect, oLocalOptions.fallbackTextKey || "");
        var sText = resolveTextValue(sTextKey, oController, oEffect, oLocalOptions);
        return FeedbackBannerRuntime.applyEffect(oState, oEffect, sText);
    }

    return {
        handleEffect: handleEffect
    };
});
