sap.ui.define([
    "checklist/app/service/framework/EffectApplier",
    "checklist/app/service/framework/EffectUiHandlers",
    "checklist/app/service/framework/FeedbackBannerRuntime"
], function (EffectApplier, EffectUiHandlers, FeedbackBannerRuntime) {
    "use strict";

    function resolveBundle(oController) {
        var oI18n = oController && oController.getModel && oController.getModel("i18n");
        return oI18n && oI18n.getResourceBundle ? oI18n.getResourceBundle() : null;
    }

    function resolveText(oController, sKey, aArgs, sFallback) {
        var oBundle = resolveBundle(oController);
        if (oBundle && oBundle.hasText && oBundle.hasText(sKey)) {
            return oBundle.getText(sKey, aArgs || []);
        }
        return sFallback || sKey || "";
    }

    function applyUseCaseResult(oController, oResult, mOptions) {
        var oUiHandlers = EffectUiHandlers.create({
            resolveTextKey: function (sKey, oCtrl) {
                return resolveText(oCtrl || oController, sKey, [], sKey);
            }
        });
        return EffectApplier.applyEffects(oController, oResult && oResult.effects, {
            resolveTextKey: function (sKey) {
                return resolveText(oController, sKey, [], sKey);
            },
            handlers: oUiHandlers,
            actionDispatcher: mOptions && mOptions.actionDispatcher
        }).then(function () {
            return oResult;
        });
    }

    function showGlobalMessage(oController, sSeverity, sTextKey, aArgs, sFallback) {
        var oState = oController && oController.getModel && oController.getModel("state");
        var sText = resolveText(oController, sTextKey, aArgs || [], sFallback || sTextKey);
        FeedbackBannerRuntime.setGlobalMessage(oState, sSeverity, sText);
        return sText;
    }

    function showToast(oController, sTextKey, aArgs, sLevel) {
        return EffectApplier.applyEffects(oController, [{
            type: "toast",
            textKey: sTextKey,
            textArgs: aArgs || [],
            level: sLevel || "info"
        }], {
            resolveTextKey: function (sKey) {
                return resolveText(oController, sKey, aArgs || [], sKey);
            }
        });
    }

    return {
        resolveText: resolveText,
        applyUseCaseResult: applyUseCaseResult,
        showGlobalMessage: showGlobalMessage,
        showToast: showToast
    };
});
