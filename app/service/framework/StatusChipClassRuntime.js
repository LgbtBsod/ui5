sap.ui.define([], function () {
    "use strict";

    var CHIP_STATE_CLASSES = [
        "chipStateSuccess",
        "chipStateWarning",
        "chipStateError",
        "chipStateInfo",
        "chipStateNone"
    ];
    var DELEGATE_KEY = "__chipStateDelegateAttached";

    function stateToClass(sState) {
        switch (String(sState || "").trim()) {
        case "Success":
            return "chipStateSuccess";
        case "Warning":
            return "chipStateWarning";
        case "Error":
            return "chipStateError";
        case "Information":
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
        if (!oControl || !oControl.addEventDelegate || !oControl.data) {
            return;
        }
        if (oControl.data(DELEGATE_KEY)) {
            return;
        }
        oControl.addEventDelegate({
            onAfterRendering: function () {
                syncControl(oControl);
            }
        });
        oControl.data(DELEGATE_KEY, "true");
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
