sap.ui.define([
    "checklist/app/service/framework/EffectApplier",
    "checklist/app/service/framework/EffectTextResolver",
    "checklist/app/service/framework/EffectUiHandlers",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (EffectApplier, EffectTextResolver, EffectUiHandlers, FeedbackBannerRuntime, BehaviorRegistry) {
    "use strict";

    var FEEDBACK_SCOPE = "feedback";
    var bDefaultsRegistered = false;

    function resolveText(mContext) {
        return EffectTextResolver.getText(
            mContext.controller,
            mContext.textKey,
            mContext.args || [],
            mContext.fallback || mContext.textKey || ""
        );
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
