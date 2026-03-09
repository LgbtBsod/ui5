sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime"
], function (ModelStateRuntime, ControllerModelRuntime) {
    "use strict";

    function resolveFromStateModel(oStateModel) {
        return String(
            ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "") ||
            ModelStateRuntime.readOnModel(oStateModel, "/selectedId", "")
        ).trim();
    }

    function resolveActiveFromStateModel(oStateModel) {
        return String(
            ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "")
        ).trim();
    }

    function resolveFromController(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
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
