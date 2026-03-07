sap.ui.define([
    "checklist/app/service/framework/ControllerViewStateRuntime"
], function (ControllerViewStateRuntime) {
    "use strict";

    function syncDrafts(oController, oSelectedModel, sModelPath) {
        var oBasic;
        if (!oController || !oController.getModel || !oController.getModel("view") || !oSelectedModel || !oSelectedModel.getProperty) {
            return;
        }
        if (sModelPath === "/") {
            oBasic = oSelectedModel.getProperty("/basic") || {};
            ControllerViewStateRuntime.set(oController, "/observerInputValue", String(oBasic.OBSERVER_FULLNAME || ""));
            ControllerViewStateRuntime.set(oController, "/observedInputValue", String(oBasic.OBSERVED_FULLNAME || ""));
            return;
        }
        if (sModelPath === "/basic/OBSERVER_FULLNAME") {
            ControllerViewStateRuntime.set(
                oController,
                "/observerInputValue",
                String(oSelectedModel.getProperty("/basic/OBSERVER_FULLNAME") || "")
            );
            return;
        }
        if (sModelPath === "/basic/OBSERVED_FULLNAME") {
            ControllerViewStateRuntime.set(
                oController,
                "/observedInputValue",
                String(oSelectedModel.getProperty("/basic/OBSERVED_FULLNAME") || "")
            );
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
