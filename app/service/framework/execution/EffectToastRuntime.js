sap.ui.define([
    "sap/m/MessageToast",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectTextResolver"
], function (MessageToast, EffectFeedbackContracts, EffectTextResolver) {
    "use strict";

    var CLASSES = EffectFeedbackContracts.CLASSES;
    var DURATIONS = EffectFeedbackContracts.DURATIONS;
    var LEVELS = EffectFeedbackContracts.LEVELS;
    var mToastTimeline = {};

    function resolveTextKey(oController, oEffect, oOptions, sFallbackKey) {
        var fnResolveTextKey = (oOptions && oOptions.resolveTextKey) || EffectTextResolver.resolve;
        var oPayload = oEffect && oEffect.payload;
        var sKey = (oEffect && oEffect.textKey) || (oPayload && oPayload.messageKey) || sFallbackKey || "";
        return sKey ? fnResolveTextKey(sKey, oController) : "";
    }

    function toast(oController, oEffect, oOptions) {
        var sText = resolveTextKey(oController, oEffect, oOptions);
        var sToastKey = String((oEffect && (oEffect.correlationKey || oEffect.textKey || sText)) || "").trim();
        var iNow = Date.now();
        var iLastShownAt = Number(mToastTimeline[sToastKey] || 0);
        if (sToastKey && Number.isFinite(iLastShownAt) && (iNow - iLastShownAt) < DURATIONS.TOAST_DEDUPE_MS) {
            return;
        }
        if (sToastKey) {
            mToastTimeline[sToastKey] = iNow;
        }
        if (sText) {
            var sLevel = String((oEffect && oEffect.level) || LEVELS.INFO).toLowerCase();
            var sClassName = [CLASSES.TOAST, CLASSES.TOAST_LEVEL_PREFIX + sLevel].join(" ");
            MessageToast.show(String(sText), {
                className: sClassName,
                duration: DURATIONS.TOAST_SHOW_MS
            });
        }
    }

    return {
        resolveTextKey: resolveTextKey,
        toast: toast
    };
});
