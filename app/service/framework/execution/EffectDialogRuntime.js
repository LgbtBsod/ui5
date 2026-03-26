sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LazyDialogRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeInput",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DialogConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntime"
], function (LazyDialogRuntime, RuntimeInput, DialogContracts, JsRuntime) {
    "use strict";

    var TYPE_FUNCTION = JsRuntime.TYPEOF.FUNCTION;
    var METHODS = JsRuntime.METHODS;

    function normalizeEffectVerb(vAction) {
        return RuntimeInput.asString(vAction).trim().toLowerCase();
    }

    function dispatchEffectAction(oOptions, oEffect) {
        var oDispatcher = oOptions && oOptions.actionDispatcher;
        var sActionName = RuntimeInput.asString(oEffect && oEffect.actionName).trim();
        var oPayload = RuntimeInput.asObject((oEffect && oEffect.payload) || {});
        if (!oDispatcher || typeof oDispatcher.dispatch !== "function" || !sActionName) {
            return Promise.resolve(false);
        }
        return oDispatcher.dispatch(sActionName, oPayload);
    }

    var DIALOG_ACTIONS = {
        open: function (oDialog) {
            if (oDialog && typeof oDialog[METHODS.OPEN] === TYPE_FUNCTION) {
                oDialog[METHODS.OPEN]();
                return true;
            }
            return false;
        },
        close: function (oDialog) {
            if (oDialog && typeof oDialog[METHODS.CLOSE] === TYPE_FUNCTION) {
                oDialog[METHODS.CLOSE]();
                return true;
            }
            return false;
        }
    };

    function resolveSharedExpandedRowsDialog(oController, sId) {
        if (sId !== DialogContracts.IDS.CHECKS_EXPANDED && sId !== DialogContracts.IDS.BARRIERS_EXPANDED) {
            return null;
        }
        return LazyDialogRuntime.resolveDialog(oController, sId, {
            dialogId: "expandedRowsDialog"
        });
    }

    function resolveDialog(oController, sId) {
        if (!oController || !sId) {
            return null;
        }
        return resolveSharedExpandedRowsDialog(oController, sId)
            || LazyDialogRuntime.resolveDialog(oController, sId, {
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
        if (oController && typeof oController.ensureEffectDialog === TYPE_FUNCTION) {
            return Promise.resolve(oController.ensureEffectDialog(sId)).then(function (oLazyDialog) {
                return oLazyDialog || resolveDialog(oController, sId);
            });
        }
        return Promise.resolve(null);
    }

    function applyDialogAction(oDialog, sAction) {
        var sNormalizedAction = normalizeEffectVerb(sAction);
        var fn = DIALOG_ACTIONS[sNormalizedAction];
        if (typeof fn !== TYPE_FUNCTION) {
            return false;
        }
        return !!fn(oDialog);
    }

    function runDialogEffect(oController, oEffect, oOptions) {
        var sAction = normalizeEffectVerb(oEffect && oEffect.action);
        if (oController && typeof oController.shouldAllowDialogEffect === TYPE_FUNCTION &&
            oController.shouldAllowDialogEffect(oEffect && oEffect.id, sAction, oEffect) === false) {
            return Promise.resolve(false);
        }
        if (sAction === "dispatch") {
            return dispatchEffectAction(oOptions, oEffect);
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
