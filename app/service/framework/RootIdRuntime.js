sap.ui.define([], function () {
    "use strict";

    function resolveFromStateModel(oStateModel) {
        return String(
            (oStateModel && oStateModel.getProperty && (
                oStateModel.getProperty("/activeObjectId") ||
                oStateModel.getProperty("/selectedId")
            )) || ""
        ).trim();
    }

    function resolveActiveFromStateModel(oStateModel) {
        return String(
            (oStateModel && oStateModel.getProperty && oStateModel.getProperty("/activeObjectId")) || ""
        ).trim();
    }

    function resolveFromController(oController) {
        var oStateModel = oController && oController.getModel && oController.getModel("state");
        return resolveFromStateModel(oStateModel);
    }

    function resolveCurrentRootId(oController) {
        return String((oController && oController._currentRootId && oController._currentRootId()) || "").trim()
            || resolveFromController(oController);
    }

    function withCurrentRootId(oController, mInput) {
        var oInput = Object.assign({}, mInput || {});
        oInput.rootId = resolveCurrentRootId(oController);
        return oInput;
    }

    return {
        resolveFromStateModel: resolveFromStateModel,
        resolveActiveFromStateModel: resolveActiveFromStateModel,
        resolveFromController: resolveFromController,
        resolveCurrentRootId: resolveCurrentRootId,
        withCurrentRootId: withCurrentRootId
    };
});
