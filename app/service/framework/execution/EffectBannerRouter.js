sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput"
], function (FeedbackBannerRuntime, RuntimeInput) {
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

    function normalizeEffectVerb(vAction) {
        return RuntimeInput.asString(vAction).trim().toLowerCase();
    }

    function dispatchEffectAction(oRuntimeOptions, oEffect) {
        var oDispatcher = oRuntimeOptions && oRuntimeOptions.actionDispatcher;
        var sActionName = RuntimeInput.asString(oEffect && oEffect.actionName).trim();
        var oPayload = RuntimeInput.asObject((oEffect && oEffect.payload) || {});
        if (!oDispatcher || typeof oDispatcher.dispatch !== "function" || !sActionName) {
            return Promise.resolve(false);
        }
        return oDispatcher.dispatch(sActionName, oPayload);
    }

    function handleEffect(oController, oEffect, oRuntimeOptions, mOptions) {
        var oState = readStateModel(oController);
        var oLocalOptions = mOptions || {};
        var sAction = normalizeEffectVerb(oEffect && oEffect.action);

        if (sAction === "dispatch") {
            return dispatchEffectAction(oRuntimeOptions, oEffect);
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
