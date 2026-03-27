sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectApplier",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectBannerRouter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/FeedbackBehaviorHelpers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FeedbackConstants"
], function (EffectApplier, EffectBannerRouter, FeedbackBehaviorHelpers, FeedbackBannerRuntime, BehaviorRegistry, FeedbackConstants) {
    "use strict";

    var FEEDBACK_SCOPE = "feedback";
    var EFFECT_TYPE_TOAST = "toast";
    var BANNER_SCOPE_ROUTE = FeedbackConstants.SCOPE.ROUTE;
    var BANNER_VISIBLE = true;
    var TOAST_LEVEL_INFO = FeedbackConstants.SEVERITY.INFO;
    var bDefaultsRegistered = false;

    function resolveText(mContext) {
        return FeedbackBehaviorHelpers.resolveText(mContext.controller, mContext.textKey, mContext.args || [], mContext.fallback || mContext.textKey);
    }

    function applyUseCaseResult(mContext) {
        var oController = mContext.controller;
        var oResult = mContext.result;
        var fnResolveText = FeedbackBehaviorHelpers.createResolver(oController);
        var oUiHandlers = {
            banner: function (oEffect, oCtrl, oOptions) {
                return EffectBannerRouter.handleEffect(oCtrl, oEffect, oOptions, {
                    fallbackTextKey: "",
                    resolveTextKey: FeedbackBehaviorHelpers.createResolver(oCtrl || oController)
                }, oOptions);
            },
            dialog: function (oEffect, oCtrl, oOptions) {
                return false;
            }
        };
        return EffectApplier.applyEffects(oController, oResult && oResult.effects, {
            resolveTextKey: fnResolveText,
            handlers: oUiHandlers,
            actionDispatcher: mContext.options && mContext.options.actionDispatcher
        }).then(function () {
            return oResult;
        });
    }

    function showGlobalMessage(mContext) {
        var oState = mContext.controller && mContext.controller.getModel ? mContext.controller.getModel("state") : null;
        var sText = resolveText({
            controller: mContext.controller,
            textKey: mContext.textKey,
            args: mContext.args || [],
            fallback: mContext.fallback || mContext.textKey
        });
        FeedbackBannerRuntime.setGlobalMessage(oState, mContext.severity, sText);
        return sText;
    }

    function showRouteMessage(mContext) {
        var oState = mContext.controller && mContext.controller.getModel ? mContext.controller.getModel("state") : null;
        var sText = resolveText({
            controller: mContext.controller,
            textKey: mContext.textKey,
            args: mContext.args || [],
            fallback: mContext.fallback || mContext.textKey
        });
        FeedbackBannerRuntime.setBanner(oState, "route", {
            visible: BANNER_VISIBLE,
            scope: BANNER_SCOPE_ROUTE,
            severity: mContext.severity,
            text: sText
        });
        return sText;
    }

    function showToast(mContext) {
        return EffectApplier.applyEffects(mContext.controller, [{
            type: EFFECT_TYPE_TOAST,
            textKey: mContext.textKey,
            textArgs: mContext.args || [],
            level: mContext.level || TOAST_LEVEL_INFO
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
        showRouteMessage: showRouteMessage,
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
