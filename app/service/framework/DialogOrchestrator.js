sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/behavior/BehaviorScopes",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectApplier"
], function (BehaviorScopes, EffectApplier) {
    "use strict";

    function runOperation(sOperation, mContext) {
        return BehaviorScopes.dialog.execute(sOperation, mContext || {});
    }

    function ensure(oController, sKey, mConfig) {
        return runOperation("ensure", {
            controller: oController,
            key: sKey,
            config: mConfig || {}
        });
    }

    function resolve(oController, sKey, mConfig) {
        return runOperation("resolve", {
            controller: oController,
            key: sKey,
            config: mConfig || {}
        });
    }

    function open(oController, sKey, mConfig) {
        return runOperation("open", {
            controller: oController,
            key: sKey,
            config: mConfig || {}
        });
    }

    function close(oController, sKey, mConfig) {
        return runOperation("close", {
            controller: oController,
            key: sKey,
            config: mConfig || {}
        });
    }

    function runEffect(oController, oEffect, oOptions) {
        return runOperation("runEffect", {
            controller: oController,
            effect: oEffect || null,
            options: oOptions || {}
        });
    }

    return {
        ensure: ensure,
        resolve: resolve,
        open: open,
        close: close,
        runEffect: runEffect,
        registerBehaviorOverride: BehaviorScopes.dialog.registerBehaviorOverride,
        unregisterBehaviorOverride: BehaviorScopes.dialog.unregisterBehaviorOverride,
        clearBehaviorOverrides: BehaviorScopes.dialog.clearBehaviorOverrides,
        promptWarning: EffectApplier.promptWarning,
        promptConfirm: EffectApplier.promptConfirm,
        promptError: EffectApplier.promptError,
        actions: EffectApplier.actions
    };
});
