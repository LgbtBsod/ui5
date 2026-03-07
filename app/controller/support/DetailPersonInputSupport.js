sap.ui.define([
    "checklist/app/controller/support/ControllerModelWriteSupport"
], function (ControllerModelWriteSupport) {
    "use strict";

    function setPersonDraft(oController, sPath, sValue) {
        ControllerModelWriteSupport.set(oController, "view", sPath, String(sValue || ""));
    }

    function syncDrafts(oController, oSelectedModel, sModelPath) {
        var oBasic;
        if (!oController || !oController.getModel || !oController.getModel("view") || !oSelectedModel || !oSelectedModel.getProperty) {
            return;
        }
        if (sModelPath === "/") {
            oBasic = oSelectedModel.getProperty("/basic") || {};
            setPersonDraft(oController, "/observerInputValue", oBasic.OBSERVER_FULLNAME);
            setPersonDraft(oController, "/observedInputValue", oBasic.OBSERVED_FULLNAME);
            return;
        }
        if (sModelPath === "/basic/OBSERVER_FULLNAME") {
            setPersonDraft(oController, "/observerInputValue", oSelectedModel.getProperty("/basic/OBSERVER_FULLNAME"));
            return;
        }
        if (sModelPath === "/basic/OBSERVED_FULLNAME") {
            setPersonDraft(oController, "/observedInputValue", oSelectedModel.getProperty("/basic/OBSERVED_FULLNAME"));
        }
    }

    function targetFromSource(oSource) {
        return (oSource && oSource.data && oSource.data("target")) || "observer";
    }

    return {
        syncDrafts: syncDrafts,
        targetFromSource: targetFromSource
    };
});
