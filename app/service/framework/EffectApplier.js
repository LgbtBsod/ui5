sap.ui.define([
    "sap/m/MessageToast",
    "sap/m/MessageBox",
    "PRODUCTION_CONTROL_CHECKLIST/util/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/Ui5StyleAdapter",
    "PRODUCTION_CONTROL_CHECKLIST/service/ui/StyleTokens",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectTextResolver",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectBannerRouter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectActionRouting",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil"
], function (MessageToast, MessageBox, DebugLogger, Ui5StyleAdapter, StyleTokens, EffectTextResolver, EffectBannerRouter, EffectActionRouting, EffectDialogRuntime, ModelStateRuntime, CloneUtil) {
    "use strict";

    var DIALOG_CLASS = "glassDialog";
    var TOAST_CLASS = "glassToast";
    var TOAST_LEVEL_CLASS_PREFIX = "glassToast--";
    var TOAST_DEDUPE_MS = 2500;
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
        if (sToastKey && Number.isFinite(iLastShownAt) && (iNow - iLastShownAt) < TOAST_DEDUPE_MS) {
            return;
        }
        if (sToastKey) {
            mToastTimeline[sToastKey] = iNow;
        }
        if (sText) {
            var sLevel = String((oEffect && oEffect.level) || "info").toLowerCase();
            var sClassName = [TOAST_CLASS, TOAST_LEVEL_CLASS_PREFIX + sLevel].join(" ");
            MessageToast.show(String(sText), {
                className: sClassName,
            duration: 2200
        });
        }
    }
    function banner(oController, oEffect, oOptions) {
        return withOptionalHandler(oController, oEffect, oOptions, "banner", function () {
            return EffectBannerRouter.handleEffect(oController, oEffect, oOptions, {
                fallbackTextKey: "loadErrorMessage",
                resolveTextKey: function (sTextKey) {
                    return resolveTextKey(oController, { textKey: sTextKey }, oOptions, "");
                }
            });
        });
    }
    function showDialog(oController, oEffect, oOptions) {
        var sVariant = String((oEffect && oEffect.variant) || "").toLowerCase();
        var sFallbackKey = oEffect && oEffect.id === "conflict" ? "conflictDialogText" : "";
        var fnShow;
        var sText = resolveTextKey(oController, oEffect, oOptions, sFallbackKey);
        if (!sText) {
            return null;
        }
        if (oEffect && oEffect.id === "conflict") {
            sVariant = "warning";
        }
        fnShow = DIALOG_VARIANT_HANDLERS[sVariant] || MessageBox.show;
        fnShow(String(sText), { styleClass: DIALOG_CLASS });
        return null;
    }
    function dialog(oController, oEffect, oOptions) {
        return withOptionalHandler(oController, oEffect, oOptions, "dialog", function () {
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
            styleClass: DIALOG_CLASS,
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
    function inlineValidation(oController, oEffect) {
        return patchModel(oController, {
            modelName: oEffect.modelName || "state",
            path: oEffect.path || "/ui/feedback/inlineErrors",
            value: oEffect.value || {}
        });
    }
    function getModel(oController, sModelName) {
        if (!oController || !oController.getModel) { return null; }
        return oController.getModel(sModelName);
    }
    function busyModel(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var sPath = oEffect.path || (oEffect.scope ? "/busy/" + oEffect.scope : "/busy");
        ModelStateRuntime.writeOnModel(oModel, sPath, !!oEffect.value);
    }
    function patchModel(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        ModelStateRuntime.writeOnModel(oModel, oEffect.path, CloneUtil.clone(oEffect.value));
    }
    function mergeModel(oController, oEffect) {
        var oModel = getModel(oController, oEffect.modelName);
        var oCurrent;
        if (!oModel || !oModel.getProperty || !oModel.setProperty) { return; }
        oCurrent = ModelStateRuntime.readOnModel(oModel, oEffect.path, {}) || {};
        ModelStateRuntime.writeOnModel(
            oModel,
            oEffect.path,
            Object.assign({}, CloneUtil.clone(oCurrent), CloneUtil.clone(oEffect.partialObject || {}))
        );
    }
    function styleTokenEnable(oController, oEffect) {
        return Ui5StyleAdapter.enable(oController, StyleTokens.resolveClassName(oEffect.token), oEffect.target || "view");
    }
    function styleTokenDisable(oController, oEffect) {
        return Ui5StyleAdapter.disable(oController, StyleTokens.resolveClassName(oEffect.token), oEffect.target || "view");
    }
    var mEffectHandlers = {
        toast: function (oController, oEffect, oOptions) { return toast(oController, oEffect, oOptions); },
        busy: function (oController, oEffect) { return busyModel(oController, oEffect); },
        modelPatch: function (oController, oEffect) { return patchModel(oController, oEffect); },
        modelMerge: function (oController, oEffect) { return mergeModel(oController, oEffect); },
        navigate: function (oController, oEffect) { return navigate(oController, oEffect); },
        banner: function (oController, oEffect, oOptions) { return banner(oController, oEffect, oOptions); },
        dialog: function (oController, oEffect, oOptions) { return dialog(oController, oEffect, oOptions); },
        confirm: function (oController, oEffect, oOptions) { return confirm(oController, oEffect, oOptions); },
        log: function (_oController, oEffect) { return log(oEffect); },
        inlineValidation: function (oController, oEffect) { return inlineValidation(oController, oEffect); },
        styleTokenEnable: function (oController, oEffect) { return styleTokenEnable(oController, oEffect); },
        styleTokenDisable: function (oController, oEffect) { return styleTokenDisable(oController, oEffect); }
    };
    function resolveEffectHandler(sType) {
        return mEffectHandlers[sType] || null;
    }
    function applyEffect(oController, oEffect, oOptions) {
        var fnHandler;
        if (!oEffect || !oEffect.type) {
            return null;
        }
        fnHandler = resolveEffectHandler(oEffect.type);
        if (typeof fnHandler !== "function") {
            return null;
        }
        return fnHandler(oController, oEffect, oOptions);
    }
    function applyEffects(oController, aEffects, oOptions) {
        return Promise.all((Array.isArray(aEffects) ? aEffects : []).map(function (oEffect) {
            return Promise.resolve(applyEffect(oController, oEffect, oOptions || {}));
        }));
    }
    function promptBox(sKind, sText, aActions, sEmphasized) {
        return new Promise(function (resolve) {
            MessageBox[sKind](String(sText || ""), {
                actions: aActions || [MessageBox.Action.OK],
                emphasizedAction: sEmphasized,
                styleClass: DIALOG_CLASS,
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
        MessageBox.error(String(sText || ""), { styleClass: DIALOG_CLASS });
    }

    return { applyEffects: applyEffects, promptWarning: promptWarning, promptConfirm: promptConfirm, promptError: promptError, actions: MessageBox.Action };
});
