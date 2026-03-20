sap.ui.define([
    "sap/m/MessageBox",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/EffectFeedbackContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/EffectDialogRuntime"
], function (MessageBox, EffectFeedbackContracts, EffectDialogRuntime) {
    "use strict";

    var CLASSES = EffectFeedbackContracts.CLASSES;
    var FALLBACK_TEXT_KEYS = EffectFeedbackContracts.FALLBACK_TEXT_KEYS;
    var IDS = EffectFeedbackContracts.IDS;
    var VARIANTS = EffectFeedbackContracts.VARIANTS;
    var DIALOG_VARIANT_HANDLERS = {
        warning: MessageBox.warning,
        information: MessageBox.information
    };

    function showDialog(oEffect, sText) {
        var sVariant = String((oEffect && oEffect.variant) || "").toLowerCase();
        var fnShow;
        if (!sText) {
            return null;
        }
        if (oEffect && oEffect.id === IDS.CONFLICT) {
            sVariant = VARIANTS.WARNING;
        }
        fnShow = DIALOG_VARIANT_HANDLERS[sVariant] || MessageBox.show;
        fnShow(String(sText), { styleClass: CLASSES.DIALOG });
        return null;
    }

    function dialog(oController, oEffect, oOptions) {
        return Promise.resolve(EffectDialogRuntime.runDialogEffect(oController, oEffect, oOptions)).then(function (vHandled) {
            if (vHandled === false) {
                return showDialog(oEffect, oOptions.resolveText(oEffect && oEffect.id === IDS.CONFLICT ? FALLBACK_TEXT_KEYS.CONFLICT_DIALOG : ""));
            }
            return null;
        });
    }

    function promptBox(sKind, sText, aActions, sEmphasized) {
        return new Promise(function (resolve) {
            MessageBox[sKind](String(sText || ""), {
                actions: aActions || [MessageBox.Action.OK],
                emphasizedAction: sEmphasized,
                styleClass: CLASSES.DIALOG,
                onClose: resolve
            });
        });
    }

    return {
        actions: MessageBox.Action,
        dialog: dialog,
        promptConfirm: function (sText, aActions, sEmphasized) { return promptBox("confirm", sText, aActions, sEmphasized); },
        promptError: function (sText) { MessageBox.error(String(sText || ""), { styleClass: CLASSES.DIALOG }); },
        promptWarning: function (sText, aActions, sEmphasized) { return promptBox("warning", sText, aActions, sEmphasized); }
    };
});
