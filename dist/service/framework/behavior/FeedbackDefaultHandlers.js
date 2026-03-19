sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectApplier",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectBannerRouter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectTextResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
], function (EffectApplier, EffectBannerRouter, EffectTextResolver, FeedbackBannerRuntime, ControllerModelRuntime, BehaviorRegistry) {
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
        var oUiHandlers = {
            banner: function (oEffect, oCtrl, oOptions) {
                return EffectBannerRouter.handleEffect(oCtrl, oEffect, oOptions, {
                    fallbackTextKey: "",
                    resolveTextKey: function (sTextKey) {
                        return resolveText({
                            controller: oCtrl || oController,
                            textKey: sTextKey,
                            args: [],
                            fallback: sTextKey
                        });
                    }
                }, oOptions);
            },
            dialog: function (oEffect, oCtrl, oOptions) {
                return false;
            }
        };
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
        var oState = ControllerModelRuntime.state(mContext.controller);
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
        var oState = ControllerModelRuntime.state(mContext.controller);
        var sText = resolveText({
            controller: mContext.controller,
            textKey: mContext.textKey,
            args: mContext.args || [],
            fallback: mContext.fallback || mContext.textKey
        });
        FeedbackBannerRuntime.setBanner(oState, "route", {
            visible: true,
            scope: "route",
            severity: mContext.severity,
            text: sText
        });
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
