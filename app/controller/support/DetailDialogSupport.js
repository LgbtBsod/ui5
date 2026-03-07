sap.ui.define([
    "sap_ui5/service/framework/DraftChecklistFactory",
    "sap_ui5/service/framework/LazyDialogRuntime"
], function (DraftChecklistFactory, LazyDialogRuntime) {
    "use strict";

    var EFFECT_DIALOG_FRAGMENTS = {
        locationValueHelp: "sap_ui5.view.fragment.LocationValueHelpDialog",
        checksExpanded: "sap_ui5.view.fragment.ChecksExpandedDialog",
        barriersExpanded: "sap_ui5.view.fragment.BarriersExpandedDialog"
    };

    function loadDialog(oController, sKey, sName) {
        return LazyDialogRuntime.ensureDialog(oController, sKey, {
            fragmentName: sName,
            afterClose: function (_oDialog, oCtrl, sDialogKey) {
                if (oCtrl && typeof oCtrl._restoreDialogFocus === "function") {
                    oCtrl._restoreDialogFocus(sDialogKey);
                }
            },
            afterOpen: function (_oDialog, oCtrl, sDialogKey) {
                if (oCtrl && typeof oCtrl._onDialogAfterOpen === "function") {
                    oCtrl._onDialogAfterOpen(sDialogKey);
                }
            }
        });
    }

    function ensureEffectDialog(oController, sId) {
        var sFragment = EFFECT_DIALOG_FRAGMENTS[sId];
        if (sFragment) {
            return loadDialog(oController, sId, sFragment);
        }
        return Promise.resolve(null);
    }

    function createEmptyDraft() {
        return DraftChecklistFactory.createEmptyDraft();
    }

    return {
        createEmptyDraft: createEmptyDraft,
        ensureEffectDialog: ensureEffectDialog
    };
});
