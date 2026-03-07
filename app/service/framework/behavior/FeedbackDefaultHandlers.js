sap.ui.define([
    "checklist/app/service/framework/EffectApplier",
    "checklist/app/service/framework/EffectUiHandlers",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (EffectApplier, EffectUiHandlers, FeedbackBannerRuntime, BehaviorRegistry) {
    "use strict";

    var FEEDBACK_SCOPE = "feedback";
    var bDefaultsRegistered = false;

    function resolveBundle(oController) {
        var oI18n = oController && oController.getModel && oController.getModel("i18n");
        return oI18n && oI18n.getResourceBundle ? oI18n.getResourceBundle() : null;
    }

    function resolveText(mContext) {
        var oBundle = resolveBundle(mContext.controller);
        if (oBundle && oBundle.hasText && oBundle.hasText(mContext.textKey)) {
            return oBundle.getText(mContext.textKey, mContext.args || []);
        }
        return mContext.fallback || mContext.textKey || "";
    }

    function applyUseCaseResult(mContext) {
        var oController = mContext.controller;
        var oResult = mContext.result;
        var oUiHandlers = EffectUiHandlers.create({
            resolveTextKey: function (sKey, oCtrl) {
                return resolveText({
                    controller: oCtrl || oController,
                    textKey: sKey,
                    args: [],
                    fallback: sKey
                });
            }
        });
        return EffectApplier.applyEffects(oController, oResult && oResult.effects, {
            resolveTextKey: function (sKey) {
                return resolveText({
                    controller: oController,
                    textKey: sKey,
                    args: [],
                    fallback: sKey
                });
            },
            handlers: oUiHandlers,
            actionDispatcher: mContext.options && mContext.options.actionDispatcher
        }).then(function () {
            return oResult;
        });
    }

    function showGlobalMessage(mContext) {
        var oState = mContext.controller && mContext.controller.getModel && mContext.controller.getModel("state");
        var sText = resolveText({
            controller: mContext.controller,
            textKey: mContext.textKey,
            args: mContext.args || [],
            fallback: mContext.fallback || mContext.textKey
        });
        FeedbackBannerRuntime.setGlobalMessage(oState, mContext.severity, sText);
        return sText;
    }

    function showToast(mContext) {
        return EffectApplier.applyEffects(mContext.controller, [{
            type: "toast",
            textKey: mContext.textKey,
            textArgs: mContext.args || [],
            level: mContext.level || "info"
        }], {
            resolveTextKey: function (sKey) {
                return resolveText({
                    controller: mContext.controller,
                    textKey: sKey,
                    args: mContext.args || [],
                    fallback: sKey
                });
            }
        });
    }

    var mHandlers = {
        resolveText: resolveText,
        applyUseCaseResult: applyUseCaseResult,
        showGlobalMessage: showGlobalMessage,
        showToast: showToast
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(FEEDBACK_SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
