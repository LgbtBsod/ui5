sap.ui.define([
    "checklist/app/service/framework/EffectBannerRouter",
    "checklist/app/service/framework/EffectDialogRuntime"
], function (EffectBannerRouter, EffectDialogRuntime) {
    "use strict";

    function create(mOptions) {
        var oOptions = mOptions || {};
        var fnResolveText = typeof oOptions.resolveTextKey === "function" ? oOptions.resolveTextKey : function () { return ""; };
        var sBannerFallbackTextKey = String(oOptions.bannerFallbackTextKey || "");

        return {
            banner: function (oEffect, oController, oRuntimeOptions) {
                return EffectBannerRouter.handleEffect(oController, oEffect, oRuntimeOptions, {
                    fallbackTextKey: sBannerFallbackTextKey,
                    resolveTextKey: function (sTextKey) {
                        return fnResolveText(sTextKey, oController);
                    }
                });
            },
            dialog: function (oEffect, oController, oRuntimeOptions) {
                return EffectDialogRuntime.runDialogEffect(oController, oEffect, oRuntimeOptions);
            }
        };
    }

    return {
        create: create
    };
});
