sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/UiSemanticConstants"
], function (UiSemanticConstants) {
    "use strict";

    var CHIP_STATE_CLASSES = [
        "chipStateSuccess",
        "chipStateWarning",
        "chipStateError",
        "chipStateInfo",
        "chipStateNone"
    ];
    var DELEGATE_KEY = "__chipStateDelegateAttached";
    var DELEGATE_REF_KEY = "__chipStateDelegateRef";

    function stateToClass(sState) {
        switch (String(sState || "").trim()) {
        case UiSemanticConstants.OBJECT_STATUS_STATE.SUCCESS:
            return "chipStateSuccess";
        case UiSemanticConstants.OBJECT_STATUS_STATE.WARNING:
            return "chipStateWarning";
        case UiSemanticConstants.OBJECT_STATUS_STATE.ERROR:
            return "chipStateError";
        case UiSemanticConstants.OBJECT_STATUS_STATE.INFORMATION:
            return "chipStateInfo";
        default:
            return "chipStateNone";
        }
    }

    function clearChipStateClasses(oControl) {
        CHIP_STATE_CLASSES.forEach(function (sClassName) {
            oControl.removeStyleClass(sClassName);
        });
    }

    function syncControl(oControl) {
        var sClassName;
        if (!oControl || !oControl.addStyleClass || !oControl.removeStyleClass) {
            return;
        }
        sClassName = stateToClass(oControl.getState && oControl.getState());
        clearChipStateClasses(oControl);
        if (sClassName) {
            oControl.addStyleClass(sClassName);
        }
    }

    function ensureDelegate(oControl) {
        var oDelegate;
        if (!oControl || !oControl.addEventDelegate || !oControl.data) {
            return;
        }
        if (oControl.data(DELEGATE_KEY)) {
            return;
        }
        oDelegate = {
            onAfterRendering: function () {
                syncControl(oControl);
            }
        };
        oControl.addEventDelegate(oDelegate);
        oControl.data(DELEGATE_KEY, "true");
        oControl.data(DELEGATE_REF_KEY, oDelegate);
    }

    function isSemanticChip(oControl) {
        return !!(oControl
            && oControl.isA
            && oControl.isA("sap.m.ObjectStatus")
            && oControl.hasStyleClass
            && oControl.hasStyleClass("statusChipSemantic"));
    }

    function syncRoot(oRoot) {
        var aControls;
        if (!oRoot || !oRoot.findAggregatedObjects) {
            return;
        }
        aControls = oRoot.findAggregatedObjects(true, isSemanticChip) || [];
        aControls.forEach(function (oControl) {
            ensureDelegate(oControl);
            syncControl(oControl);
        });
    }

    function syncView(oController) {
        var oView = oController && oController.getView && oController.getView();
        syncRoot(oView);
    }

    return {
        syncControl: syncControl,
        syncRoot: syncRoot,
        syncView: syncView
    };
});
