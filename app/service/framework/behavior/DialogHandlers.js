sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LazyDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/behavior/BehaviorRegistry"
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
        defaults: {
            handlers: mHandlers,
            ensureRegistered: ensureRegistered
        },
        overrides: {
            ensureRegistered: function () {
                return true;
            },
            register: function (sId, fnHandler) {
                return BehaviorRegistry.registerOverride(DIALOG_SCOPE, sId, fnHandler);
            },
            unregister: function (sId) {
                return BehaviorRegistry.unregisterOverride(DIALOG_SCOPE, sId);
            },
            clear: function () {
                return BehaviorRegistry.clearOverrides(DIALOG_SCOPE);
            }
        }
    };
});
