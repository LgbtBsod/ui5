sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (ControllerModelRuntime, ModelStateRuntime, ModelPathContracts, DetailContracts) {
    "use strict";

    var DETAIL_MODEL_PATHS = DetailContracts.MODEL_PATHS;

    function sanitize(vValue) {
        return String(vValue || "").trim();
    }

    function firstNonEmpty() {
        var iIndex;
        for (iIndex = 0; iIndex < arguments.length; iIndex += 1) {
            if (sanitize(arguments[iIndex])) {
                return sanitize(arguments[iIndex]);
            }
        }
        return "";
    }

    function resolveFromController(oController) {
        var oStateModel = ControllerModelRuntime.state(oController);
        var oDetailModel = ControllerModelRuntime.detail(oController);

        return firstNonEmpty(
            ModelStateRuntime.readOnModel(oDetailModel, DETAIL_MODEL_PATHS.ROOT_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "")
        );
    }

    function resolveActiveFromStateModel(oStateModel) {
        return firstNonEmpty(
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.SELECTED_ID, ""),
            ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.POST_OPEN_HYDRATED_ROOT_ID, "")
        );
    }

    return Object.freeze({
        resolveFromController: resolveFromController,
        resolveActiveFromStateModel: resolveActiveFromStateModel
    });
});
