sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LazyDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectActionRouting"
], function (LazyDialogRuntime, EffectActionRouting) {
    "use strict";

    var DIALOG_ACTIONS = {
        open: function (oDialog) {
            if (oDialog && typeof oDialog.open === "function") {
                oDialog.open();
                return true;
            }
            return false;
        },
        close: function (oDialog) {
            if (oDialog && typeof oDialog.close === "function") {
                oDialog.close();
                return true;
            }
            return false;
        }
    };

    function resolveDialog(oController, sId) {
        if (!oController || !sId) {
            return null;
        }
        return LazyDialogRuntime.resolveDialog(oController, sId, {
            dialogId: sId + "Dialog"
        }) || LazyDialogRuntime.resolveDialog(oController, sId, {
            dialogId: sId
        });
    }

    function resolveDialogAsync(oController, sId) {
        var oDialog = resolveDialog(oController, sId);
        if (oDialog) {
            return Promise.resolve(oDialog);
        }
        if (oController && typeof oController.ensureEffectDialog === "function") {
            return Promise.resolve(oController.ensureEffectDialog(sId)).then(function (oLazyDialog) {
                return oLazyDialog || resolveDialog(oController, sId);
            });
        }
        return Promise.resolve(null);
    }

    function applyDialogAction(oDialog, sAction) {
        var sNormalizedAction = EffectActionRouting.normalizeEffectVerb(sAction);
        var fn = DIALOG_ACTIONS[sNormalizedAction];
        if (typeof fn !== "function") {
            return false;
        }
        return !!fn(oDialog);
    }

    function runDialogEffect(oController, oEffect, oOptions) {
        var sAction = EffectActionRouting.normalizeEffectVerb(oEffect && oEffect.action);
        if (oController && typeof oController.shouldAllowDialogEffect === "function" &&
            oController.shouldAllowDialogEffect(oEffect && oEffect.id, sAction, oEffect) === false) {
            return Promise.resolve(false);
        }
        if (sAction === "dispatch") {
            return EffectActionRouting.dispatchEffectAction(oController, oOptions, oEffect);
        }
        return resolveDialogAsync(oController, oEffect && oEffect.id).then(function (oDialog) {
            if (!oDialog) {
                return false;
            }
            return applyDialogAction(oDialog, sAction);
        });
    }

    return {
        resolveDialog: resolveDialog,
        resolveDialogAsync: resolveDialogAsync,
        applyDialogAction: applyDialogAction,
        runDialogEffect: runDialogEffect
    };
});
