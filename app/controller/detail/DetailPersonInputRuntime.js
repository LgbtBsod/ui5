sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerViewStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/runtime/DetailPersonContracts"
], function (ModelStateRuntime, ControllerViewStateRuntime, ControllerModelRuntime, ModelContracts, DetailPersonContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var SELECTED_MODEL = MODELS.SELECTED;
    var INPUT_PATHS = DetailPersonContracts.INPUT_PATHS;
    var MODEL_PATHS = DetailPersonContracts.MODEL_PATHS;
    var TARGETS = DetailPersonContracts.TARGETS;

    function syncDrafts(oController, oSelectedModel, sModelPath) {
        var oBasic;
        if (!oController || !ControllerModelRuntime.viewState(oController) || !oSelectedModel || !oSelectedModel.getProperty) {
            return;
        }
        if (sModelPath === "/") {
            oBasic = ModelStateRuntime.read(oController, SELECTED_MODEL, "/basic", {}) || {};
            ControllerViewStateRuntime.set(oController, INPUT_PATHS.OBSERVER, String(oBasic.OBSERVER_FULLNAME || ""));
            ControllerViewStateRuntime.set(oController, INPUT_PATHS.OBSERVED, String(oBasic.OBSERVED_FULLNAME || ""));
            return;
        }
        if (sModelPath === MODEL_PATHS.OBSERVER_FULLNAME) {
            ControllerViewStateRuntime.set(
                oController,
                INPUT_PATHS.OBSERVER,
                String(ModelStateRuntime.read(oController, SELECTED_MODEL, MODEL_PATHS.OBSERVER_FULLNAME, "") || "")
            );
            return;
        }
        if (sModelPath === MODEL_PATHS.OBSERVED_FULLNAME) {
            ControllerViewStateRuntime.set(
                oController,
                INPUT_PATHS.OBSERVED,
                String(ModelStateRuntime.read(oController, SELECTED_MODEL, MODEL_PATHS.OBSERVED_FULLNAME, "") || "")
            );
        }
    }

    function targetFromSource(oSource) {
        return (oSource && oSource.data && oSource.data("target")) || TARGETS.OBSERVER;
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
        var sKey = String(sTarget || TARGETS.OBSERVER).toLowerCase();
        if (!mState) {
            return;
        }
        mState[sKey] = String(sValue || "");
    }

    function consumeSuggestionSelection(oController, sTarget, sValue) {
        var mState = ensureSelectionState(oController);
        var sKey = String(sTarget || TARGETS.OBSERVER).toLowerCase();
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
