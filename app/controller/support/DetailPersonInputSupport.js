sap.ui.define([
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/ControllerViewStateRuntime",
    "checklist/app/service/framework/ControllerModelRuntime"
], function (ModelStateRuntime, ControllerViewStateRuntime, ControllerModelRuntime) {
    "use strict";

    function syncDrafts(oController, oSelectedModel, sModelPath) {
        var oBasic;
        if (!oController || !ControllerModelRuntime.viewState(oController) || !oSelectedModel || !oSelectedModel.getProperty) {
            return;
        }
        if (sModelPath === "/") {
            oBasic = ModelStateRuntime.read(oController, "selected", "/basic", {}) || {};
            ControllerViewStateRuntime.set(oController, "/observerInputValue", String(oBasic.OBSERVER_FULLNAME || ""));
            ControllerViewStateRuntime.set(oController, "/observedInputValue", String(oBasic.OBSERVED_FULLNAME || ""));
            return;
        }
        if (sModelPath === "/basic/OBSERVER_FULLNAME") {
            ControllerViewStateRuntime.set(
                oController,
                "/observerInputValue",
                String(ModelStateRuntime.read(oController, "selected", "/basic/OBSERVER_FULLNAME", "") || "")
            );
            return;
        }
        if (sModelPath === "/basic/OBSERVED_FULLNAME") {
            ControllerViewStateRuntime.set(
                oController,
                "/observedInputValue",
                String(ModelStateRuntime.read(oController, "selected", "/basic/OBSERVED_FULLNAME", "") || "")
            );
        }
    }

    function targetFromSource(oSource) {
        return (oSource && oSource.data && oSource.data("target")) || "observer";
    }

    function ensureSelectionState(oController) {
        if (!oController) {
            return null;
        }
        if (!oController._mPendingPersonSuggestionSelection) {
            oController._mPendingPersonSuggestionSelection = {};
        }
        return oController._mPendingPersonSuggestionSelection;
    }

    function rememberSuggestionSelection(oController, sTarget, sValue) {
        var mState = ensureSelectionState(oController);
        var sKey = String(sTarget || "observer").toLowerCase();
        if (!mState) {
            return;
        }
        mState[sKey] = String(sValue || "");
    }

    function consumeSuggestionSelection(oController, sTarget, sValue) {
        var mState = ensureSelectionState(oController);
        var sKey = String(sTarget || "observer").toLowerCase();
        var sExpected;
        if (!mState) {
            return false;
        }
        sExpected = String(mState[sKey] || "");
        if (!sExpected || sExpected !== String(sValue || "")) {
            return false;
        }
        delete mState[sKey];
        return true;
    }

    return {
        syncDrafts: syncDrafts,
        targetFromSource: targetFromSource,
        rememberSuggestionSelection: rememberSuggestionSelection,
        consumeSuggestionSelection: consumeSuggestionSelection
    };
});
