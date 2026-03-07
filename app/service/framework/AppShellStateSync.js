sap.ui.define([], function () {
    "use strict";

    function resolveStateModel(oController) {
        return oController.getModel("state") || (oController.getOwnerComponent && oController.getOwnerComponent() && oController.getOwnerComponent().getModel("state"));
    }

    function ensureControllerStateModel(oController, oStateModel) {
        if (oStateModel && !oController.getModel("state")) { oController.getView().setModel(oStateModel, "state"); }
    }

    return {
        resolveStateModel: resolveStateModel,
        ensureControllerStateModel: ensureControllerStateModel
    };
});
