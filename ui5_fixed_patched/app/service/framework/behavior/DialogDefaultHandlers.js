sap.ui.define([
    "checklist/app/service/framework/LazyDialogRuntime",
    "checklist/app/service/framework/EffectDialogRuntime",
    "checklist/app/service/framework/behavior/BehaviorRegistry"
], function (LazyDialogRuntime, EffectDialogRuntime, BehaviorRegistry) {
    "use strict";

    var DIALOG_SCOPE = "dialog";
    var bDefaultsRegistered = false;

    function ensure(mContext) {
        return LazyDialogRuntime.ensureDialog(mContext.controller, mContext.key, mContext.config || {});
    }

    function resolve(mContext) {
        return LazyDialogRuntime.resolveDialog(mContext.controller, mContext.key, mContext.config || {});
    }

    function open(mContext) {
        return ensure(mContext).then(function (oDialog) {
            if (oDialog && typeof oDialog.open === "function") {
                oDialog.open();
            }
            return oDialog || null;
        });
    }

    function close(mContext) {
        var oDialog = resolve(mContext);
        if (oDialog && typeof oDialog.close === "function") {
            oDialog.close();
            return true;
        }
        return false;
    }

    function runEffect(mContext) {
        return EffectDialogRuntime.runDialogEffect(mContext.controller, mContext.effect, mContext.options || {});
    }

    var mHandlers = {
        ensure: ensure,
        resolve: resolve,
        open: open,
        close: close,
        runEffect: runEffect
    };

    function ensureRegistered() {
        if (bDefaultsRegistered) {
            return;
        }
        Object.keys(mHandlers).forEach(function (sOperation) {
            BehaviorRegistry.registerDefault(DIALOG_SCOPE, sOperation, mHandlers[sOperation]);
        });
        bDefaultsRegistered = true;
    }

    return {
        handlers: mHandlers,
        ensureRegistered: ensureRegistered
    };
});
