sap.ui.define([
    "checklist/app/service/framework/behavior/BehaviorResolver",
    "checklist/app/service/framework/behavior/DialogDefaultHandlers",
    "checklist/app/service/framework/behavior/DialogOverrideHandlers",
    "checklist/app/service/framework/EffectApplier"
], function (BehaviorResolver, DialogDefaultHandlers, DialogOverrideHandlers, EffectApplier) {
    "use strict";

    function runOperation(sOperation, mContext) {
        DialogDefaultHandlers.ensureRegistered();
        DialogOverrideHandlers.ensureRegistered();
        return BehaviorResolver.execute("dialog", sOperation, mContext || {}, DialogDefaultHandlers.handlers);
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
        registerBehaviorOverride: DialogOverrideHandlers.register,
        unregisterBehaviorOverride: DialogOverrideHandlers.unregister,
        clearBehaviorOverrides: DialogOverrideHandlers.clear,
        promptWarning: EffectApplier.promptWarning,
        promptConfirm: EffectApplier.promptConfirm,
        promptError: EffectApplier.promptError,
        actions: EffectApplier.actions
    };
});
