sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectToastRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectBannerRouter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectActionRouting",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectDialogFeedbackRuntime"
], function (DebugLogger, EffectFeedbackContracts, EffectToastRuntime, EffectBannerRouter, EffectActionRouting, EffectDialogFeedbackRuntime) {
    "use strict";

    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;
    var HANDLER_NAMES = EffectFeedbackContracts.EFFECT_HANDLER_NAMES;
    var resolveTextKey = EffectToastRuntime.resolveTextKey;

    function withOptionalHandler(oController, oEffect, oOptions, sHandlerName, fnDefault) {
        var oHandlers = oOptions && oOptions.handlers;
        var fnHandler = oHandlers && oHandlers[sHandlerName];
        if (typeof fnHandler !== "function") {
            return fnDefault();
        }
        return Promise.resolve(fnHandler(oEffect, oController, oOptions)).then(function (vHandled) {
            return vHandled === false ? fnDefault() : null;
        });
    }

    function navigate(oController, oEffect) {
        var oRouter = oController && oController.getRouter ? oController.getRouter() : null;
        if (oRouter && oRouter.navTo) {
            oRouter.navTo(oEffect.route, oEffect.params || {}, !!oEffect.replace);
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
                    return resolveTextKey(oController, { textKey: sTextKey }, oOptions, "");
                }
            });
        });
    }

    function dialog(oController, oEffect, oOptions) {
        return withOptionalHandler(oController, oEffect, oOptions, HANDLER_NAMES.DIALOG, function () {
            return EffectDialogFeedbackRuntime.dialog(oController, oEffect, {
                resolveText: function (sFallbackKey) {
                    return resolveTextKey(oController, oEffect, oOptions, sFallbackKey || FALLBACK_TEXT_KEYS.CONFLICT_DIALOG);
                }
            });
        });
    }

    function confirm(oController, oEffect, oOptions) {
        var sText = resolveTextKey(oController, oEffect, oOptions);
        var oPayload = oEffect.payload || {};
        var sConfirm = oPayload.confirmText || EffectDialogFeedbackRuntime.actions.YES;
        var sCancel = oPayload.cancelText || EffectDialogFeedbackRuntime.actions.NO;
        return EffectDialogFeedbackRuntime.promptConfirm(String(sText || ""), [sConfirm, sCancel], sConfirm).then(function (sAction) {
            var sYes = oPayload.confirmAction;
            var sNo = oPayload.cancelAction;
            var oDispatchPayload = EffectActionRouting.resolveActionPayload({ payload: oPayload });
            var sActionName = "";
            if (sAction === sConfirm) {
                sActionName = sYes;
            } else if (sAction === sCancel) {
                sActionName = sNo;
            }
            EffectActionRouting.dispatchByName(oController, oOptions, sActionName, oDispatchPayload);
        });
    }

    function log(oEffect) {
        if (DebugLogger && typeof DebugLogger.info === "function") {
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
