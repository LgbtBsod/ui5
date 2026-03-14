sap.ui.define([
"PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectToastRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectBannerRouter",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectActionRouting",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectDialogFeedbackRuntime",
    "sap/ui/core/routing/HashChanger",
    "sap/ui/core/Component"
], function (DebugLogger, EffectFeedbackContracts, EffectToastRuntime, EffectBannerRouter, EffectActionRouting, EffectDialogFeedbackRuntime, HashChanger, UIComponent) {
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

    function resolveRouter(oController) {
        var oOwnerComponent = oController && typeof oController.getOwnerComponent === "function" ? oController.getOwnerComponent() : null;
        var oRouter = oController && typeof oController.getRouter === "function" ? oController.getRouter() : null;
        var oComponentOwner;
        var oAppView;

        if (oRouter && typeof oRouter.navTo === "function") {
            return oRouter;
        }
        if (oOwnerComponent && typeof oOwnerComponent.getRouter === "function") {
            oRouter = oOwnerComponent.getRouter();
            if (oRouter && typeof oRouter.navTo === "function") {
                return oRouter;
            }
        }
        if (UIComponent && typeof UIComponent.getOwnerComponentFor === "function") {
            oComponentOwner = UIComponent.getOwnerComponentFor(oController && typeof oController.getView === "function" ? oController.getView() : null);
            if (oComponentOwner && typeof oComponentOwner.getRouter === "function") {
                oRouter = oComponentOwner.getRouter();
                if (oRouter && typeof oRouter.navTo === "function") {
                    return oRouter;
                }
            }
        }
        if (typeof sap !== "undefined" && sap.ui && typeof sap.ui.component === "function") {
            oOwnerComponent = sap.ui.component("checklist_app_comp");
            if (oOwnerComponent && typeof oOwnerComponent.getRouter === "function") {
                oRouter = oOwnerComponent.getRouter();
                if (oRouter && typeof oRouter.navTo === "function") {
                    return oRouter;
                }
            }
        }
        if (typeof sap !== "undefined" && sap.ui && sap.ui.getCore && typeof sap.ui.getCore().byId === "function") {
            oAppView = sap.ui.getCore().byId("checklist_app_comp---app");
            if (oAppView && typeof oAppView.getController === "function") {
                oRouter = oAppView.getController() && typeof oAppView.getController().getRouter === "function"
                    ? oAppView.getController().getRouter()
                    : null;
                if (oRouter && typeof oRouter.navTo === "function") {
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
        if (oRouter && oEffect && oEffect.replace && typeof oRouter.getURL === "function") {
            sUrl = String(oRouter.getURL(oEffect.route, oEffect.params || {}) || "");
            oHashChanger = HashChanger && HashChanger.getInstance ? HashChanger.getInstance() : null;
            if (oHashChanger && typeof oHashChanger.replaceHash === "function") {
                oHashChanger.replaceHash(String(sUrl || "").replace(/^\/?/, ""));
                return;
            }
            if (typeof window !== "undefined" && window.location) {
                window.location.hash = sUrl ? "#/" + String(sUrl || "").replace(/^\/+/, "") : "";
                return;
            }
        }
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
