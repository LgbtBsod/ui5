sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectModelRuntime"
], function (EffectFeedbackRuntime, EffectModelRuntime) {
    "use strict";

    var mEffectHandlers = {
        toast: function (oController, oEffect, oOptions) { return EffectFeedbackRuntime.toast(oController, oEffect, oOptions); },
        busy: function (oController, oEffect) { return EffectModelRuntime.busyModel(oController, oEffect); },
        modelPatch: function (oController, oEffect) { return EffectModelRuntime.patchModel(oController, oEffect); },
        modelMerge: function (oController, oEffect) { return EffectModelRuntime.mergeModel(oController, oEffect); },
        navigate: function (oController, oEffect) { return EffectFeedbackRuntime.navigate(oController, oEffect); },
        banner: function (oController, oEffect, oOptions) { return EffectFeedbackRuntime.banner(oController, oEffect, oOptions); },
        dialog: function (oController, oEffect, oOptions) { return EffectFeedbackRuntime.dialog(oController, oEffect, oOptions); },
        confirm: function (oController, oEffect, oOptions) { return EffectFeedbackRuntime.confirm(oController, oEffect, oOptions); },
        log: function (_oController, oEffect) { return EffectFeedbackRuntime.log(oEffect); },
        inlineValidation: function (oController, oEffect) { return EffectModelRuntime.inlineValidation(oController, oEffect); },
        styleTokenEnable: function (oController, oEffect) { return EffectModelRuntime.styleTokenEnable(oController, oEffect); },
        styleTokenDisable: function (oController, oEffect) { return EffectModelRuntime.styleTokenDisable(oController, oEffect); }
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

    return {
        applyEffects: applyEffects,
        promptWarning: EffectFeedbackRuntime.promptWarning,
        promptConfirm: EffectFeedbackRuntime.promptConfirm,
        promptError: EffectFeedbackRuntime.promptError,
        actions: EffectFeedbackRuntime.actions
    };
});
