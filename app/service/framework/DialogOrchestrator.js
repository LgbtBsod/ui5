sap.ui.define([
    "checklist/app/service/framework/LazyDialogRuntime",
    "checklist/app/service/framework/EffectDialogRuntime",
    "checklist/app/service/framework/EffectApplier"
], function (LazyDialogRuntime, EffectDialogRuntime, EffectApplier) {
    "use strict";

    function ensure(oController, sKey, mConfig) {
        return LazyDialogRuntime.ensureDialog(oController, sKey, mConfig || {});
    }

    function resolve(oController, sKey, mConfig) {
        return LazyDialogRuntime.resolveDialog(oController, sKey, mConfig || {});
    }

    function open(oController, sKey, mConfig) {
        return ensure(oController, sKey, mConfig).then(function (oDialog) {
            if (oDialog && typeof oDialog.open === "function") {
                oDialog.open();
            }
            return oDialog || null;
        });
    }

    function close(oController, sKey, mConfig) {
        var oDialog = resolve(oController, sKey, mConfig);
        if (oDialog && typeof oDialog.close === "function") {
            oDialog.close();
            return Promise.resolve(true);
        }
        return Promise.resolve(false);
    }

    function runEffect(oController, oEffect, oOptions) {
        return EffectDialogRuntime.runDialogEffect(oController, oEffect, oOptions || {});
    }

    return {
        ensure: ensure,
        resolve: resolve,
        open: open,
        close: close,
        runEffect: runEffect,
        promptWarning: EffectApplier.promptWarning,
        promptConfirm: EffectApplier.promptConfirm,
        promptError: EffectApplier.promptError,
        actions: EffectApplier.actions
    };
});
