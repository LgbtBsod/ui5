sap.ui.define([
    "checklist/app/service/framework/DraftChecklistFactory",
    "checklist/app/service/framework/LazyDialogRuntime"
], function (DraftChecklistFactory, LazyDialogRuntime) {
    "use strict";

    var EFFECT_DIALOG_FRAGMENTS = {
        locationValueHelp: "checklist.app.view.fragment.LocationValueHelpDialog",
        checksExpanded: "checklist.app.view.fragment.ChecksExpandedDialog",
        barriersExpanded: "checklist.app.view.fragment.BarriersExpandedDialog"
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
