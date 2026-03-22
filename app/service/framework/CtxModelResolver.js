sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime"
], function (
    ControllerModelRuntime
) {
    "use strict";

    function resolveDirectModel(oHost, sName) {
        if (!oHost || typeof oHost.getModel !== "function") {
            return null;
        }
        return oHost.getModel(sName);
    }

    function collectModels(oController) {
        var oView;

        if (!oController) {
            return {};
        }

        oView = ControllerModelRuntime.view(oController);

        return {
            default: ControllerModelRuntime.defaultModel(oController) || resolveDirectModel(oController),
            view: ControllerModelRuntime.viewState(oController) || (oView && oView.getModel ? oView.getModel("view") : null),
            state: ControllerModelRuntime.state(oController) || resolveDirectModel(oController, "state"),
            detail: ControllerModelRuntime.detail(oController) || resolveDirectModel(oController, "detail"),
            shell: ControllerModelRuntime.shell(oController) || resolveDirectModel(oController, "shell"),
            masterData: ControllerModelRuntime.masterData(oController) || resolveDirectModel(oController, "masterData")
        };
    }

    return {
        collectModels: collectModels
    };
});
