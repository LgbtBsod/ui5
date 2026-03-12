sap.ui.define([
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "PRODUCTION_CONTROL_CHECKLIST/util/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectTextResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectBannerRouter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectActionRouting",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectDialogRuntime"
], function (MessageToast, MessageBox, DebugLogger, EffectTextResolver, EffectFeedbackContracts, EffectBannerRouter, EffectActionRouting, EffectDialogRuntime) {
    "use strict";

    var CLASSES = EffectFeedbackContracts.CLASSES;
    var DURATIONS = EffectFeedbackContracts.DURATIONS;
    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;
    var HANDLER_NAMES = EffectFeedbackContracts.EFFECT_HANDLER_NAMES;
    var IDS = EffectFeedbackContracts.IDS;
    var LEVELS = EffectFeedbackContracts.LEVELS;
    var VARIANTS = EffectFeedbackContracts.VARIANTS;
    var DIALOG_VARIANT_HANDLERS = {
        warning: MessageBox.warning,
        information: MessageBox.information
    };
    var mToastTimeline = {};

    function resolveTextKey(oController, oEffect, oOptions, sFallbackKey) {
        var fnResolveTextKey = (oOptions && oOptions.resolveTextKey) || EffectTextResolver.resolve;
        var oPayload = oEffect && oEffect.payload;
        var sKey = (oEffect && oEffect.textKey) || (oPayload && oPayload.messageKey) || sFallbackKey || "";
        return sKey ? fnResolveTextKey(sKey, oController) : "";
    }

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

    function showDialog(oController, oEffect, oOptions) {
        var sVariant = String((oEffect && oEffect.variant) || "").toLowerCase();
        var sFallbackKey = oEffect && oEffect.id === IDS.CONFLICT ? FALLBACK_TEXT_KEYS.CONFLICT_DIALOG : "";
        var fnShow;
        var sText = resolveTextKey(oController, oEffect, oOptions, sFallbackKey);
        if (!sText) {
            return null;
        }
        if (oEffect && oEffect.id === IDS.CONFLICT) {
            sVariant = VARIANTS.WARNING;
        }
        fnShow = DIALOG_VARIANT_HANDLERS[sVariant] || MessageBox.show;
        fnShow(String(sText), { styleClass: CLASSES.DIALOG });
        return null;
    }

    function dialog(oController, oEffect, oOptions) {
        return withOptionalHandler(oController, oEffect, oOptions, HANDLER_NAMES.DIALOG, function () {
            return Promise.resolve(EffectDialogRuntime.runDialogEffect(oController, oEffect, oOptions)).then(function (vHandled) {
                if (vHandled === false) {
                    return showDialog(oController, oEffect, oOptions);
                }
                return null;
            });
        });
    }

    function confirm(oController, oEffect, oOptions) {
        var sText = resolveTextKey(oController, oEffect, oOptions);
        var oPayload = oEffect.payload || {};
        var sConfirm = oPayload.confirmText || MessageBox.Action.YES;
        var sCancel = oPayload.cancelText || MessageBox.Action.NO;
        MessageBox.confirm(String(sText || ""), {
            actions: [sConfirm, sCancel],
            emphasizedAction: sConfirm,
            styleClass: CLASSES.DIALOG,
            onClose: function (sAction) {
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
            }
        });
    }

    function log(oEffect) {
        if (DebugLogger && typeof DebugLogger.info === "function") {
            DebugLogger.info("UseCase", oEffect.level + ": " + (oEffect.message || ""), oEffect.meta || {});
        }
    }

    function promptBox(sKind, sText, aActions, sEmphasized) {
        return new Promise(function (resolve) {
            MessageBox[sKind](String(sText || ""), {
                actions: aActions || [MessageBox.Action.OK],
                emphasizedAction: sEmphasized,
                styleClass: CLASSES.DIALOG,
                onClose: resolve
            });
        });
    }

    function promptWarning(sText, aActions, sEmphasized) {
        return promptBox("warning", sText, aActions, sEmphasized);
    }

    function promptConfirm(sText, aActions, sEmphasized) {
        return promptBox("confirm", sText, aActions, sEmphasized);
    }

    function promptError(sText) {
        MessageBox.error(String(sText || ""), { styleClass: CLASSES.DIALOG });
    }

    return {
        navigate: navigate,
        toast: toast,
        banner: banner,
        dialog: dialog,
        confirm: confirm,
        log: log,
        promptWarning: promptWarning,
        promptConfirm: promptConfirm,
        promptError: promptError,
        actions: MessageBox.Action
    };
});
