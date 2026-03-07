sap.ui.define([
    "checklist/app/service/framework/EffectApplier",
    "checklist/app/service/framework/EffectUiHandlers",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/controller/base/ControllerTextRuntime"
], function (EffectApplier, EffectUiHandlers, FeedbackBannerRuntime, ControllerTextRuntime) {
    "use strict";

    function resolveTextKey(sKey, oController) {
        return ControllerTextRuntime.getText(oController, sKey, [], sKey);
    }

    return {
        showI18nToast: function (sI18nKey, aArgs) {
            var oState = this.getModel("state");
            var sText = ControllerTextRuntime.getText(this, sI18nKey, aArgs || [], sI18nKey);
            FeedbackBannerRuntime.setGlobalMessage(oState, "info", sText);
        },

        showI18nError: function (sI18nKey, aArgs) {
            var oState = this.getModel("state");
            var sText = ControllerTextRuntime.getText(this, sI18nKey, aArgs || [], sI18nKey);
            FeedbackBannerRuntime.setGlobalMessage(oState, "error", sText);
        },

        applyUseCaseEffects: function (oResult) {
            var oUiHandlers = EffectUiHandlers.create({
                resolveTextKey: resolveTextKey
            });
            return EffectApplier.applyEffects(this, oResult && oResult.effects, {
                resolveTextKey: resolveTextKey,
                handlers: oUiHandlers
            }).then(function () {
                return oResult;
            });
        },

        executeFacadeMethod: function (oFacade, sMethod, mInput, mCtx) {
            var fn = oFacade && oFacade[sMethod];
            if (typeof fn !== "function") {
                return Promise.resolve();
            }
            return Promise.resolve(fn.call(oFacade, mInput || {}, mCtx || {})).then(function (oResult) {
                return this.applyUseCaseEffects(oResult);
            }.bind(this));
        }
    };
});

