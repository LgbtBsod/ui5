sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectApplier",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectFeedbackRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectModelRuntime"
], function (EffectApplier, EffectFeedbackRuntime, EffectModelRuntime) {
    "use strict";

    function applyEffects(oController, aEffects, oOptions) {
        return EffectApplier.applyEffects(oController, aEffects, oOptions || {});
    }

    function applyEffect(oController, oEffect, oOptions) {
        return EffectApplier.applyEffects(oController, [oEffect], oOptions || {});
    }

    return Object.freeze({
        applyEffect: applyEffect,
        applyEffects: applyEffects,
        feedback: EffectFeedbackRuntime,
        model: EffectModelRuntime
    });
});
