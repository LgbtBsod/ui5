sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectToastRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectBannerRouter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectActionRouting",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectDialogFeedbackRuntime",
    "sap/ui/core/routing/HashChanger",
    "sap/ui/core/Component",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/JsRuntime"
], function (DebugLogger, EffectFeedbackContracts, EffectToastRuntime, EffectBannerRouter, EffectActionRouting, EffectDialogFeedbackRuntime, HashChanger, UIComponent, JsRuntime) {
    "use strict";

    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;
    var HANDLER_NAMES = EffectFeedbackContracts.EFFECT_HANDLER_NAMES;
    var resolveTextKey = EffectToastRuntime.resolveTextKey;
    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;
    var HASH_CHANGER = JsRuntime.HASH_CHANGER;

    function resolveEffectText(oController, oEffect, oOptions, sFallbackKey) {
        return resolveTextKey(oController, oEffect, oOptions, sFallbackKey || "");
    }

    function withOptionalHandler(oController, oEffect, oOptions, sHandlerName, fnDefault) {
        var oHandlers = oOptions && oOptions.handlers;
        var fnHandler = oHandlers && oHandlers[sHandlerName];
        if (typeof fnHandler !== TYPE_FUNCTION) {
            return fnDefault();
        }
        return Promise.resolve(fnHandler(oEffect, oController, oOptions)).then(function (vHandled) {
            return vHandled === false ? fnDefault() : null;
        });
    }

    function resolveRouter(oController) {
        var oOwnerComponent = oController && typeof oController.getOwnerComponent === TYPE_FUNCTION ? oController.getOwnerComponent() : null;
        var oRouter = oController && typeof oController.getRouter === TYPE_FUNCTION ? oController.getRouter() : null;
        var oComponentOwner;

        if (oRouter && typeof oRouter[METHODS.NAV_TO] === TYPE_FUNCTION) {
            return oRouter;
        }
        if (oOwnerComponent && typeof oOwnerComponent.getRouter === TYPE_FUNCTION) {
            oRouter = oOwnerComponent.getRouter();
            if (oRouter && typeof oRouter[METHODS.NAV_TO] === TYPE_FUNCTION) {
                return oRouter;
            }
        }
        if (UIComponent && typeof UIComponent.getOwnerComponentFor === TYPE_FUNCTION) {
            oComponentOwner = UIComponent.getOwnerComponentFor(oController && typeof oController.getView === TYPE_FUNCTION ? oController.getView() : null);
            if (oComponentOwner && typeof oComponentOwner.getRouter === TYPE_FUNCTION) {
                oRouter = oComponentOwner.getRouter();
                if (oRouter && typeof oRouter[METHODS.NAV_TO] === TYPE_FUNCTION) {
                    return oRouter;
                }
            }
        }
        return null;
    }

    function navigate(oController, oEffect) {
        var oRouter = resolveRouter(oController);
        var oHashChanger;
        var sUrl;
        if (oRouter && oEffect && oEffect.replace && typeof oRouter[METHODS.GET_URL] === TYPE_FUNCTION) {
            sUrl = String(oRouter[METHODS.GET_URL](oEffect.route, oEffect.params || {}) || "");
            oHashChanger = HashChanger && HashChanger.getInstance ? HashChanger.getInstance() : null;
            if (oHashChanger && typeof oHashChanger[HASH_CHANGER.REPLACE_HASH] === TYPE_FUNCTION) {
                oHashChanger[HASH_CHANGER.REPLACE_HASH](String(sUrl || "").replace(/^\/?/, ""));
                return;
            }
        }
        if (oRouter && typeof oRouter[METHODS.NAV_TO] === TYPE_FUNCTION) {
            oRouter[METHODS.NAV_TO](oEffect.route, oEffect.params || {}, !!oEffect.replace);
        }
    }

    function toast(oController, oEffect, oOptions) {
        return EffectToastRuntime.toast(oController, oEffect, oOptions);
    }

    function banner(oController, oEffect, oOptions) {
        return withOptionalHandler(oController, oEffect, oOptions, HANDLER_NAMES.BANNER, function () {
            return EffectBannerRouter.handleEffect(oController, oEffect, oOptions, {
                fallbackTextKey: FALLBACK_TEXT_KEYS.LOAD_ERROR,
                resolveTextKey: function (sTextKey) {
                    return resolveEffectText(oController, { textKey: sTextKey }, oOptions);
                }
            });
        });
    }

    function dialog(oController, oEffect, oOptions) {
        return withOptionalHandler(oController, oEffect, oOptions, HANDLER_NAMES.DIALOG, function () {
            return EffectDialogFeedbackRuntime.dialog(oController, oEffect, {
                resolveText: function (sFallbackKey) {
                    return resolveEffectText(oController, oEffect, oOptions, sFallbackKey || FALLBACK_TEXT_KEYS.CONFLICT_DIALOG);
                }
            });
        });
    }

    function dispatchConfirmAction(oController, oOptions, oPayload, sAction, sConfirm, sCancel) {
        var sActionName = "";
        var oDispatchPayload = EffectActionRouting.resolveActionPayload({ payload: oPayload });
        if (sAction === sConfirm) {
            sActionName = oPayload.confirmAction;
        } else if (sAction === sCancel) {
            sActionName = oPayload.cancelAction;
        }
        EffectActionRouting.dispatchByName(oController, oOptions, sActionName, oDispatchPayload);
    }

    function confirm(oController, oEffect, oOptions) {
        var sText = resolveEffectText(oController, oEffect, oOptions);
        var oPayload = oEffect.payload || {};
        var sConfirm = oPayload.confirmText || EffectDialogFeedbackRuntime.actions.YES;
        var sCancel = oPayload.cancelText || EffectDialogFeedbackRuntime.actions.NO;
        return EffectDialogFeedbackRuntime.promptConfirm(String(sText || ""), [sConfirm, sCancel], sConfirm).then(function (sAction) {
            dispatchConfirmAction(oController, oOptions, oPayload, sAction, sConfirm, sCancel);
        });
    }

    function log(oEffect) {
        if (DebugLogger && typeof DebugLogger.info === TYPE_FUNCTION) {
            DebugLogger.info("UseCase", oEffect.level + ": " + (oEffect.message || ""), oEffect.meta || {});
        }
    }

    return {
        navigate: navigate,
        toast: toast,
        banner: banner,
        dialog: dialog,
        confirm: confirm,
        log: log,
        promptWarning: EffectDialogFeedbackRuntime.promptWarning,
        promptConfirm: EffectDialogFeedbackRuntime.promptConfirm,
        promptError: EffectDialogFeedbackRuntime.promptError,
        actions: EffectDialogFeedbackRuntime.actions
    };
});
